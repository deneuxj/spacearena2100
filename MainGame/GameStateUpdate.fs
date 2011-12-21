module SpaceArena2100.GameStateUpdate

open Microsoft.Xna.Framework
open CleverRake.XnaUtils

open Units
open GameState

/// Synchronization message types sent over the network
type RemoteEvent =
    | DamageAndImpulse of int<GPI> * float32<Health> * TypedVector3<m/s> // Ship idx, damage, impulse
    | BulletDestroyed of int<BulletGuid> // Bullet GUID
    | ShipState of int<GPI> * Vector3 * Vector3 * Vector3 * Vector3 * Vector3 // Ship idx, position, heading, right, speed, thrust
    | BulletFired of int<BulletGuid> * int<GPI> * float32<m> * TypedVector3<m> * TypedVector3<m/s> // Bullet GUID, owner, radius, position, speed
    | SupplySpawn of int<GSI> * Vector3 * float<m> * SupplyType // Supply idx, position, radius, type
    | SupplyGrabbed of int<GSI> // Supply idx

/// A synchronization event with the time at which it was sent.
type TimedRemoteEvent =
    { time : int<dms>;
      event : RemoteEvent }

/// Create new bullets according to BulletFired events
let createBullets now (bullets : Bullets) events =
    let events =
        events
        |> Array.filter (function { event = BulletFired _ } -> true | _ -> false)

    let newGuids =
        events
        |> Array.map (function { event = BulletFired(guid, _, _, _, _) } -> guid | _ -> failwith "Unreachable")

    let newOwners =
        events
        |> Array.map (function { event = BulletFired(_, owner, _, _, _) } -> owner | _ -> failwith "Unreachable")

    let newRadii =
        events
        |> Array.map (function { event = BulletFired(_, _, radius, _, _) } -> radius | _ -> failwith "Unreachable")

    let newPos =
        let inline mult (a, b) = CleverRake.XnaUtils.Speed3.(*) (a, b)
        events
        |> Array.map (function { time = t0; event = BulletFired(_, _, _, pos, speed) } -> pos + mult(int2float32((now - t0) / (dmsPerS)), speed) | _ -> failwith "Unreachable")

    let newSpeeds =
        events
        |> Array.map (function { event = BulletFired(_, _, _, _, speed) } -> speed | _ -> failwith "Unreachable")

    let newTimeLeft =
        events
        |> Array.map (function { time = t0 } -> now - t0)

    { guids = Array.append bullets.guids newGuids;
      owners = Array.append bullets.owners newOwners;
      radii = Array.append bullets.radii newRadii;
      speeds = Array.append bullets.speeds newSpeeds;
      timeLeft = Array.append bullets.timeLeft newTimeLeft;
      pos = Array.append bullets.pos newPos }

/// Mutate speed and array events as requested by DamageAndImpulse entries in a list of events.
let applyDamageAndImpulse speeds health events =
    for e in events do
        match e with
        | { event = DamageAndImpulse (idx, damage, impulse) } ->
            MarkedArray.mutate ((+) impulse) (speeds, idx)
            MarkedArray.mutate (fun x -> x - damage) (health, idx)
        | _ -> ()

/// Remove bullets as requested by BulletDestroyed events.
let destroyBullets bullets events =
    let guidsToRetire =
        events
        |> Array.choose(function { event = BulletDestroyed guid } -> Some guid | _ -> None)
        |> Set.ofArray

    let newOwners =
        bullets.owners
        |> ArrayInlined.filterRef (guidsToRetire.Contains >> not) bullets.guids

    let newRadii =
        bullets.radii
        |> ArrayInlined.filterRef (guidsToRetire.Contains >> not) bullets.guids

    let newSpeeds =
        bullets.speeds
        |> ArrayInlined.filterRef (guidsToRetire.Contains >> not) bullets.guids

    let newGuids =
        bullets.guids
        |> Array.filter (guidsToRetire.Contains >> not)

    let newTimeLeft =
        bullets.timeLeft
        |> ArrayInlined.filterRef (guidsToRetire.Contains >> not) bullets.guids

    let newPos =
        bullets.pos
        |> ArrayInlined.filterRef (guidsToRetire.Contains >> not) bullets.guids

    { guids = newGuids;
      owners = newOwners;
      radii = newRadii;
      speeds = newSpeeds;
      timeLeft = newTimeLeft;
      pos = newPos }

/// Merge two bullet groups
let appendBullets bullets1 bullets2 =
    { guids = Array.append bullets1.guids bullets2.guids;
      owners = Array.append bullets1.owners bullets2.owners;
      radii = Array.append bullets1.radii bullets2.radii;
      speeds = Array.append bullets1.speeds bullets2.speeds;
      timeLeft = Array.append bullets1.timeLeft bullets2.timeLeft;
      pos = Array.append bullets1.pos bullets2.pos }

/// Get a list of ship indices and asteroid indices corresponding to ships colliding with asteroids.
/// Only deals with local ships.
let computeCrashes dt (asteroids : Asteroids) (ships : Ships) localShips shipTypes =
    let getIntersection (sphere : BoundingSphere) =
        let intersected = ref None
        Octree.checkIntersection
            (fun (bbox : BoundingBox) -> bbox.Intersects(sphere))
            (fun idxAsteroid ->
                let astSphere = new BoundingSphere(asteroids.pos.[idxAsteroid].v, float32 asteroids.radius.[idxAsteroid])
                if astSphere.Intersects(sphere) then
                    intersected := Some idxAsteroid
                    true
                else
                    false)
            asteroids.octree
        |> ignore

        !intersected

    let result = new System.Collections.Generic.List<_>()
    for shipIdx in localShips do
        let speed = ships.speeds.[shipIdx]
        let pos = ships.posClient.[shipIdx]
        let course = Speed3.op_Multiply(dt, speed)
        let courseLength = course.v.Length() * 1.0f<m>
        let shipType : ShipType = MarkedArray.get shipTypes shipIdx
        let radius = shipType.BoundingSphereRadius
        assert (radius > 0.0f<m>)
        if courseLength > radius then
            let courseUnit = course.v / float32 courseLength
            let mutable offset = 0.0f<m>
            let mutable found = false
            while not found && offset <= courseLength do
                let sphere = new BoundingSphere(pos.v + (float32 offset) * courseUnit, float32 radius)
                match getIntersection sphere with
                | Some idxAsteroid -> result.Add((shipIdx, idxAsteroid)); found <- true
                | None -> ()
                offset <- offset + radius
        else
            let sphere = new BoundingSphere(pos.v, float32 radius)
            match getIntersection sphere with
            | Some idxAsteroid -> result.Add((shipIdx, idxAsteroid));
            | None -> ()

    result
    |> Seq.toList
