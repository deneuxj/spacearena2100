﻿module SpaceArena2100.GameStateUpdate

open Microsoft.Xna.Framework
open CleverRake.XnaUtils

open Units
open GameState
open WarpCoord

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
        events
        |> Array.map (function { time = t0; event = BulletFired(_, _, _, pos, speed) } -> pos + int2float32((now - t0) / (dmsPerS)) * speed | _ -> failwith "Unreachable")

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


/// Check intersection between a moving sphere and asteroids
let intersectSphereVsAsteroids V dt (asteroids : Asteroids) (pos : TypedVector3<m>) (speed : TypedVector3<m/s>) (radius : float32<m>) =
    let warp = warp V
    let getIntersection direction (sphere : BoundingSphere) =
        let intersected = ref None
        Octree.checkIntersection
            (fun (bbox : BoundingBox) -> bbox.Intersects(sphere))
            (fun idxAsteroid ->
                let astPos = asteroids.pos.[idxAsteroid]
                if Vector3.Dot(astPos.v - sphere.Center, direction) >= 0.0f then
                    let astSphere = new BoundingSphere(astPos.v, float32 asteroids.radius.[idxAsteroid])
                    if astSphere.Intersects(sphere) then
                        intersected := Some idxAsteroid
                        true
                    else
                        false
                else
                    false)
            asteroids.octree
        |> ignore

        !intersected

    let course = dt * speed
    let courseLength = course.v.Length() * 1.0f<m>
    assert (radius > 0.0f<m>)
    if courseLength > radius then
        let courseUnit = course.v / float32 courseLength
        let rec work offset =
            if offset <= courseLength then
                let sphere = new BoundingSphere(pos.v + (float32 offset) * courseUnit |> warp, float32 radius)
                match getIntersection speed.v sphere with
                | Some idxAsteroid -> Some idxAsteroid
                | None -> work (offset + radius)
            else
                None
        work 0.0f<m>
    else
        let sphere = new BoundingSphere(pos.v |> warp, float32 radius)
        getIntersection speed.v sphere


/// Get an array of ship indices and asteroid indices corresponding to ships colliding with asteroids.
/// Only deals with local ships.
let computeCrashes V dt (asteroids : Asteroids) (ships : Ships) shipTypes localShips =
    [|
        for shipIdx in localShips do
            let speed = ships.speeds.[shipIdx]
            let pos = ships.posClient.[shipIdx]
            let shipType : ShipType = MarkedArray.get shipTypes shipIdx
            let radius = shipType.BoundingSphereRadius
            match intersectSphereVsAsteroids V dt asteroids pos speed radius with
            | Some astIdx -> yield (shipIdx, astIdx)
            | None -> ()            
    |]


/// Get an array of bullet guids and ship indices corresponding to local bullet hits on ships.
/// Hits are checked against the visible position of ships.
let computeHits guidIsLocal dt (ships : Ships) shipTypes (bullets : Bullets) =
    [|
        for bulletIdx in 0 .. bullets.guids.Length - 1 do
            let guid = bullets.guids.[bulletIdx]
            if guidIsLocal guid then
                let speed = bullets.speeds.[bulletIdx]
                let pos = bullets.pos.[bulletIdx]
                let radius = bullets.radii.[bulletIdx]
                assert (radius > 0.0f<m>)
                let course = dt * speed
                let courseLength = course.v.Length() * 1.0f<m>
                let courseUnit = course.v / float32 courseLength

                for shipIdx in int ships.posVisible.First .. int ships.posVisible.Last do
                    let shipIdx = shipIdx * 1<GPI>
                    let pos' = ships.posVisible.[shipIdx]
                    let shipType : ShipType = MarkedArray.get shipTypes shipIdx
                    let radius' = shipType.BoundingSphereRadius
                    assert (radius' > 0.0f<m>)
                    let shipSphere = new BoundingSphere(pos'.v, float32 radius')

                    let rec work offset =
                        if offset <= courseLength then
                            let bulletSphere = new BoundingSphere(pos.v + (float32 offset) * courseUnit, float32 radius)
                            if bulletSphere.Intersects(shipSphere) then
                                Some (shipIdx, guid, pos, speed, radius)
                            else
                                work (offset + radius)
                        else
                            None
                    match work 0.0f<m> with
                    | Some hit -> yield hit
                    | None -> ()
    |]


/// Compute an array of bullet guids and asteroid indices denoting the bullets colliding with asteroids.
/// Only local bullets are considered.
let computeBulletAsteroidHits V guidIsLocal dt (asteroids : Asteroids) (bullets : Bullets) =
    [|
        for bulletIdx in 0 .. bullets.guids.Length - 1 do
            let guid = bullets.guids.[bulletIdx]
            if guidIsLocal guid then
                let speed = bullets.speeds.[bulletIdx]
                let pos = bullets.pos.[bulletIdx]
                let radius = bullets.radii.[bulletIdx]
                assert (radius > 0.0f<m>)
                match intersectSphereVsAsteroids V dt asteroids pos speed radius with
                | Some astIdx -> yield (guid, astIdx)
                | None -> ()
    |]


/// Compute the change in speed of two spherical colliding bodies.
let computeImpulse response pos1 speed1 massInv1 pos2 speed2 massInv2 =
    let checkCollisionSpeeds (pos1 : TypedVector3<m>) (pos2 : TypedVector3<m>) (speed1 : TypedVector3<m/s>) (speed2 : TypedVector3<m/s>) =
        let d = pos2 - pos1
        Vector3.Dot ((speed2 - speed1).v, d.v) < 0.0f


    let collisionFactor
        response pos1 pos2 vel1 vel2 (massInv1 : float32</kg>) (massInv2 : float32</kg>) =
    
        if checkCollisionSpeeds pos1 pos2 vel1 vel2
        then
            let v12 = vel1 - vel2
            let n = (pos1 - pos2) |> TypedVector.normalize3

            let x =
                (1.0f + response) * TypedVector.dot3 (v12, n) /
                (TypedVector.dot3 (n, n) * (massInv1 + massInv2))
            x
        else
            0.0f<kg m/s>

    let n = pos1 - pos2 |> TypedVector.normalize3
    
    let s = collisionFactor response pos1 pos2 speed1 speed2 massInv1 massInv2
    
    let imp1 = -s * massInv1 * n
    let imp2 = s * massInv2 * n
    
    imp1, imp2

/// Compute impulses and damages to ships due to their collisions with asteroids
let computeCrashResponse (asteroids : Asteroids) (ships : Ships) (shipTypes : MarkedArray<GPI, ShipType>) crashes =
    let response (shipIdx : int<GPI>, astIdx : int<AstIdx>) =
        let shipType = shipTypes.[shipIdx]
        let impulse, _ =
            computeImpulse
                (shipType.CollisionRestitution)
                ships.posVisible.[shipIdx] ships.speeds.[shipIdx] (shipType.InversedMass)
                asteroids.pos.[astIdx] (TypedVector3<m/s>()) (0.0f</kg>)
        let damage = shipType.Fragility * impulse.Length
        (shipIdx, impulse, damage)

    Array.map response crashes

/// Compute impules and damages due to bullet hits in ships.
let computeHitResponse (ships : Ships) (shipTypes : MarkedArray<GPI, ShipType>) hits =
    let K = 4.0f / 3.0f * Microsoft.Xna.Framework.MathHelper.Pi

    let response (shipIdx : int<GPI>, _, bulletPos : TypedVector3<m>, bulletSpeed : TypedVector3<m/s>, bulletRadius : float32<m>) =
        let shipType = shipTypes.[shipIdx]
        let r3 = bulletRadius * bulletRadius * bulletRadius
        let bulletMass = K * r3 * bulletDensity
        let impulse, _ =
            computeImpulse
                (shipType.CollisionRestitution)
                ships.posVisible.[shipIdx] ships.speeds.[shipIdx] (shipType.InversedMass)
                bulletPos bulletSpeed (1.0f / bulletMass)
        let damage = shipType.Fragility * impulse.Length
        (shipIdx, impulse, damage)

    Array.map response hits

/// Remove bullets which have expired.
let retireBullets (bullets : Bullets) =
    let filter x =
        x
        |> ArrayInlined.filterRef ((>=) 0<dms>) bullets.timeLeft

    { guids = filter bullets.guids;
      pos = filter bullets.pos;
      speeds = filter bullets.speeds;
      radii = filter bullets.radii;
      owners = filter bullets.owners;
      timeLeft = filter bullets.timeLeft }

/// Update the position and speed of all ships.
let integrateShips dt (ships : Ships) (shipTypes : MarkedArray<GPI, ShipType>) (forces : MarkedArray<GPI, _>) : Ships =
    let inline getInversedMass shipIdx =
        shipTypes.[shipIdx].InversedMass

    let speeds =
        Array.mapi2
            (fun idx speed (force : TypedVector3<N>) ->
                let shipIdx = idx * 1<GPI>
                let accel : TypedVector3<m/s^2> = getInversedMass shipIdx * force
                let speed : TypedVector3<m/s> = speed + dt * accel
                speed)
            ships.speeds.Content forces.Content

    let posClient =
        Array.map2
            (fun pos speed -> pos + dt * speed)
            ships.posClient.Content
            speeds

    let posHost =
        Array.map2
            (fun pos speed -> pos + dt * speed)
            ships.posHost.Content
            speeds

    let posVisible =
        ArrayInlined.map3
            (fun (posHost : TypedVector3<m>) (posClient : TypedVector3<m>) (t : float32) -> (1.0f - t) * posHost + t * posClient)
            ships.posHost.Content
            ships.posClient.Content
            ships.posLerpT.Content

    { ships with
        speeds = MarkedArray speeds
        posClient = MarkedArray posClient
        posHost = MarkedArray posHost
        posVisible = MarkedArray posVisible }


/// Update the position of all bullets.
let integrateBullets dt (bullets : Bullets) =
    let newPos =
        Array.map2 (fun pos speed -> pos + dt * speed) bullets.pos bullets.speeds

    { bullets with
        pos = newPos }