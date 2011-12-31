﻿module SpaceArena2100.GameStateUpdate

open Microsoft.Xna.Framework
open CleverRake.XnaUtils

open Units
open GameState
open WarpCoord

module BulletConstants =
    let speed = 300.0f<m/s>
    let fastSpeed = 500.0f<m/s>
    let lifetime = 2.0f<s>
    let radius = 0.2f<m>
    let bigRadius = 0.3f<m>
    let firePeriod = 0.25f<s>
    let highRateFirePeriod = 0.1f<s>
    let multiFireSpread = MathHelper.ToRadians(5.0f) * 1.0f<rad>

/// Synchronization message types sent over the network
type RemoteEvent =
    | DamageAndImpulse of int<GPI> * float32<Health> * TypedVector3<m/s> // Ship idx, damage, impulse
    | BulletDestroyed of int<BulletGuid> // Bullet GUID
    | ShipState of int<GPI> * TypedVector3<m> * TypedVector3<1> * TypedVector3<1> * TypedVector3<m/s> * TypedVector3<N> // Ship idx, position, heading, right, speed, thrust
    | BulletFired of int<BulletGuid> * int<GPI> * float32<m> * TypedVector3<m> * TypedVector3<m/s> // Bullet GUID, owner, radius, position, speed
    | SupplySpawn of int<GSI> * Vector3 * float<m> * SupplyType // Supply idx, position, radius, type
    | SupplyGrabbed of int<GSI> // Supply idx

/// A synchronization event with the time at which it was sent.
type TimedRemoteEvent =
    { time : int<dms>;
      event : RemoteEvent }

/// Create new bullets according to BulletFired events
let createBullets guidIsLocal now lastLocalGuid events =
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
        let lifeLength = BulletConstants.lifetime |> dmsFromS
        events
        |> Array.map (function { time = t0 } -> lifeLength - (now - t0))

    let lastLocalGuid =
        newGuids
        |> Array.fold (fun lastGuid guid -> if guidIsLocal guid && guid > lastGuid then guid else lastGuid) lastLocalGuid

    { guids = newGuids;
      owners = newOwners;
      radii = newRadii;
      speeds = newSpeeds;
      timeLeft = newTimeLeft;
      pos = newPos;
      lastLocalGuid = lastLocalGuid }

/// Mutate speed and health arrays.
let applyDamageAndImpulse speeds health (damagesAndImpulses : (int<GPI> * TypedVector3<m/s> * float32<Health>)[]) =
    for (idx, impulse, damage) in damagesAndImpulses do
        MarkedArray.mutate ((+) impulse) (speeds, idx)
        MarkedArray.mutate (fun x -> x - damage) (health, idx)

/// Remove bullets as requested by BulletDestroyed events.
let destroyBullets bullets (guidsToRetire : Set<int<BulletGuid>>) =

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
      pos = newPos;
      lastLocalGuid = bullets.lastLocalGuid }


/// Merge two bullet groups
let appendBullets bullets1 bullets2 =
    { guids = Array.append bullets1.guids bullets2.guids;
      owners = Array.append bullets1.owners bullets2.owners;
      radii = Array.append bullets1.radii bullets2.radii;
      speeds = Array.append bullets1.speeds bullets2.speeds;
      timeLeft = Array.append bullets1.timeLeft bullets2.timeLeft;
      pos = Array.append bullets1.pos bullets2.pos
      lastLocalGuid = max bullets1.lastLocalGuid bullets2.lastLocalGuid }


/// Check intersection between a moving sphere and asteroids
let intersectSphereVsAsteroids (dt : float32<s>) (asteroids : Asteroids) (pos : TypedVector3<m>) (speed : TypedVector3<m/s>) (radius : float32<m>) =
    let warp = warp (asteroids.fieldSizes.v)
    let getIntersection direction (sphere : BoundingSphere) =
        let spherePos = sphere.Center
        let sphere = BoundingSphere(warp spherePos, sphere.Radius)

        let intersected = ref None
        let inline check idxAsteroid =
            let astPos = asteroids.pos.[idxAsteroid]
            if Vector3.Dot(warp (astPos.v - spherePos), direction) >= 0.0f then
                let astSphere = new BoundingSphere(astPos.v, float32 asteroids.radius.[idxAsteroid])
                if astSphere.Intersects(sphere) then
                    intersected := Some idxAsteroid
                    true
                else
                    false
            else
                false

        Octree.checkIntersection
            (fun (bbox : BoundingBox) -> sphere.Intersects(bbox))
            (ArrayInlined.exists check)
            asteroids.octree
        |> ignore

        !intersected

    let course = dt * speed
    let courseLength = course.Length
    assert (radius > 0.0f<m>)
    if courseLength > radius then
        let courseUnit = TypedVector.normalize3 course
        let rec work offset =
            if offset <= courseLength then
                let sphere = new BoundingSphere((pos + offset * courseUnit).v, float32 radius)
                match getIntersection speed.v sphere with
                | Some _ as v-> v
                | None -> work (offset + radius)
            else
                None
        work 0.0f<m>
    else
        let sphere = new BoundingSphere(pos.v, float32 radius)
        getIntersection speed.v sphere


/// Get an array of ship indices and asteroid indices corresponding to ships colliding with asteroids.
/// Only deals with local ships.
let computeCrashes dt (asteroids : Asteroids) (ships : Ships) shipTypes localShips =
    [|
        for shipIdx in localShips do
            let speed = ships.speeds.[shipIdx]
            let pos = ships.posClient.[shipIdx]
            let shipType : ShipType = MarkedArray.get shipTypes shipIdx
            let radius = shipType.BoundingSphereRadius
            match intersectSphereVsAsteroids dt asteroids pos speed radius with
            | Some astIdx -> yield (shipIdx, astIdx)
            | None -> ()            
    |]


/// Get an array of bullet guids and ship indices corresponding to local bullet hits on ships.
/// Hits are checked against the visible position of ships.
let computeHits guidIsLocal (dt : float32<s>) (ships : Ships) shipTypes (bullets : Bullets) =
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
let computeBulletAsteroidHits guidIsLocal dt (asteroids : Asteroids) (bullets : Bullets) =
    [|
        for bulletIdx in 0 .. bullets.guids.Length - 1 do
            let guid = bullets.guids.[bulletIdx]
            if guidIsLocal guid then
                let speed = bullets.speeds.[bulletIdx]
                let pos = bullets.pos.[bulletIdx]
                let radius = bullets.radii.[bulletIdx]
                assert (radius > 0.0f<m>)
                match intersectSphereVsAsteroids dt asteroids pos speed radius with
                | Some astIdx -> yield (guid, astIdx)
                | None -> ()
    |]


/// Compute the change in speed of two spherical colliding bodies.
let computeImpulse warp response (pos1 : TypedVector3<m>) (speed1 : TypedVector3<m/s>) (massInv1 : float32</kg>) pos2 speed2 massInv2 =
    let d = TypedVector3<m>(warp (pos2 - pos1).v)
    let n = d |> TypedVector.normalize3

    let checkCollisionSpeeds =
        Vector3.Dot ((speed2 - speed1).v, d.v) < 0.0f

    let collisionFactor =    
        if checkCollisionSpeeds
        then
            let v12 = speed1 - speed2

            let x =
                (1.0f + response) * TypedVector.dot3 (v12, n) /
                (TypedVector.dot3 (n, n) * (massInv1 + massInv2))
            x
        else
            0.0f<kg m/s>

    let s = collisionFactor
    
    let imp1 = -s * massInv1 * n
    let imp2 = s * massInv2 * n
    
    imp1, imp2

/// Compute impulses and damages to ships due to their collisions with asteroids
let computeCrashResponse (asteroids : Asteroids) (ships : Ships) (shipTypes : MarkedArray<GPI, ShipType>) crashes =
    let response (shipIdx : int<GPI>, astIdx : int<AstIdx>) =
        let shipType = shipTypes.[shipIdx]
        let impulse, _ =
            computeImpulse
                (WarpCoord.warp asteroids.fieldSizes.v)
                shipType.CollisionRestitution
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
                id
                shipType.CollisionRestitution
                ships.posVisible.[shipIdx] ships.speeds.[shipIdx] shipType.InversedMass
                bulletPos bulletSpeed (1.0f / bulletMass)
        let damage = shipType.Fragility * impulse.Length
        (shipIdx, impulse, damage)

    Array.map response hits

/// Remove bullets which have expired.
let retireBullets (bullets : Bullets) =
    let filter x =
        x
        |> ArrayInlined.filterRef (fun y -> y > 0<dms>) bullets.timeLeft

    { guids = filter bullets.guids;
      pos = filter bullets.pos;
      speeds = filter bullets.speeds;
      radii = filter bullets.radii;
      owners = filter bullets.owners;
      timeLeft = filter bullets.timeLeft;
      lastLocalGuid = bullets.lastLocalGuid }

/// Update the position and speed of all ships.
let integrateShips (dt : float32<s>) (ships : Ships) (shipTypes : MarkedArray<GPI, ShipType>) (forces : TypedVector3<N> list) localHeadings localRights (localPlayers : int<GPI> list): Ships =
    let inline getInversedMass shipIdx =
        shipTypes.[shipIdx].InversedMass

    let dtInDms = intFromFloat32 (dt * dmsPerS_f)

    let accels = ships.accels.Content |> Array.copy |> MarkedArray
    let headings = ships.headings.Content |> Array.copy |> MarkedArray
    let rights = ships.rights.Content |> Array.copy |> MarkedArray

    // Set accelerations and headings of local players
    let rec work localPlayers (forces : TypedVector3<N> list) localHeadings localRights =
        match localPlayers, forces, localHeadings, localRights with
        | shipIdx :: restPlayers, force :: restForces, heading :: restHeadings, right :: restRights ->
            let accel : TypedVector3<m/s^2> = getInversedMass shipIdx * force
            accels.[shipIdx] <- accel
            headings.[shipIdx] <- heading
            rights.[shipIdx] <- right
            work restPlayers restForces restHeadings restRights
        | [], [], [], [] ->
            ()
        | _ -> failwith "List lengths mismatch"

    work localPlayers forces localHeadings localRights

    let speeds2 =
        Array.mapi2
            (fun idx speed (accel : TypedVector3<m/s^2>) ->
                let shipIdx = idx * 1<GPI>
                let speed : TypedVector3<m/s> = speed + dt * accel
                speed)
            ships.speeds.Content accels.Content

    let posClient =
        ArrayInlined.map3
            (fun pos speed speed2-> pos + 0.5f * dt * (speed + speed2))
            ships.posClient.Content
            ships.speeds.Content
            speeds2

    let posHost =
        ArrayInlined.map3
            (fun pos speed speed2 -> pos + 0.5f * dt * (speed + speed2))
            ships.posHost.Content
            ships.speeds.Content
            speeds2

    let posVisible =
        ArrayInlined.map3
            (fun (posHost : TypedVector3<m>) (posClient : TypedVector3<m>) (t : float32) -> (1.0f - t) * posHost + t * posClient)
            ships.posHost.Content
            ships.posClient.Content
            ships.posLerpT.Content

    { ships with
        accels = accels
        headings = headings
        rights = rights
        speeds = MarkedArray speeds2
        posClient = MarkedArray posClient
        posHost = MarkedArray posHost
        posVisible = MarkedArray posVisible
        timeBeforeFire = ships.timeBeforeFire |> List.map (fun x -> x - dtInDms) }

/// Mutate posHost and the interpolation parameter posLerpT depending on sync events.
let updateDeadReckoning frameDt (now : int<dms>) (ships : Ships) (shipTypes : MarkedArray<GPI, ShipType>) (events : TimedRemoteEvent[]) =
    let lerpTime = 0.3f<s>
    let updateOfShip =
        events
        |> Array.choose (
            function
            | { time = t; event = ShipState (shipIdx, pos, heading, right, speed, force) } -> Some (shipIdx, (t, pos, heading, right, speed, force))
            | _ -> None)
        |> Map.ofSeq

    for shipIdx in ships.headings.First .. 1<GPI> .. ships.headings.Last do
        match Map.tryFind shipIdx updateOfShip with
        | Some (time, pos, heading, right, speed, force) ->
            // Update posHost, speed and accel by integrating from the timestamp of the update event.
            // The old posHost is moved to posClient.
            ships.posClient.[shipIdx] <- ships.posHost.[shipIdx]
            let dt = int2float32 ((now - time) / dmsPerS)
            let massInv = shipTypes.[shipIdx].InversedMass
            let accel = massInv * force
            let speed2 = speed + dt * accel
            let pos = pos + 0.5f * dt * (speed + speed2)
            ships.posClient.[shipIdx] <- ships.posHost.[shipIdx]
            ships.posHost.[shipIdx] <- pos
            ships.speeds.[shipIdx] <- speed2
            ships.accels.[shipIdx] <- accel
            ships.posLerpT.[shipIdx] <- 0.0f
            // Orientation attributes are copied
            ships.headings.[shipIdx] <- heading
            ships.rights.[shipIdx] <- right
        | None ->
            // No update for this ship, update the interpolation parameter.
            let newLerpT = min 1.0f (ships.posLerpT.[shipIdx] + frameDt / lerpTime)
            ships.posLerpT.[shipIdx] <- newLerpT

/// Update the position of all bullets.
let integrateBullets dt (bullets : Bullets) =
    let newPos =
        Array.map2 (fun pos speed -> pos + dt * speed) bullets.pos bullets.speeds

    let newTimeLeft =
        let dt = dmsFromS dt
        bullets.timeLeft
        |> Array.map (fun timeLeft -> max 0<dms> (timeLeft - dt))

    { bullets with
        pos = newPos
        timeLeft = newTimeLeft }

/// Check if a bullet guid was produced by a specific host.
let guidIsLocal hostNumber (guid : int<BulletGuid>) =
    (int guid &&& 0xFF) = hostNumber

/// Get the next bullet guid number.
let nextGuid last =
    last + 256<BulletGuid>

/// Get the first bullet guid of a given host.
let firstGuid hostNumber = 1<BulletGuid> * hostNumber

let stop() =
    ()

/// Update the game state.
let update dt (description : Description) events forces headings rights (state : State) =
    if events |> Array.filter (function { event = BulletFired _ } -> true | _ -> false) |> Array.isEmpty |> not then
        stop()

    let guidIsLocal = guidIsLocal (description.myHostId)
    
    updateDeadReckoning dt state.time state.ships description.shipTypes events

    let bullets =
        createBullets guidIsLocal state.time state.bullets.lastLocalGuid events
        |> appendBullets state.bullets
    
    let bulletHitsOnShips = computeHits guidIsLocal dt state.ships description.shipTypes state.bullets
    let bulletHitsOnAsteroids = computeBulletAsteroidHits guidIsLocal dt description.asteroids state.bullets
    let crashes = computeCrashes dt description.asteroids state.ships description.shipTypes description.localPlayersIdxs
    
    let damagesDueToHits = computeHitResponse state.ships description.shipTypes bulletHitsOnShips
    let damagesDueToCrashes = computeCrashResponse description.asteroids state.ships description.shipTypes crashes

    let guidsRemotelyDestroyed =
        events
        |> Array.choose(function { event = BulletDestroyed guid } -> Some guid | _ -> None)
        |> Set.ofArray

    let guidsDestroyedOnHits =
        bulletHitsOnShips
        |> Seq.map (fun (_, guid, _, _, _) -> guid)
        |> Set.ofSeq

    let guidsDestroyedOnAsteroids =
        bulletHitsOnAsteroids
        |> Seq.map fst
        |> Set.ofSeq

    let guidsToRemove =
        Set.unionMany [ guidsRemotelyDestroyed ; guidsDestroyedOnAsteroids ; guidsDestroyedOnHits ]

    let bullets = destroyBullets bullets guidsToRemove
    let bullets = retireBullets bullets
    let bullets = integrateBullets dt bullets

    let remoteDamages =
        events
        |> Array.choose (
            function
            | { event = DamageAndImpulse (idx, damage, impulse) } -> Some (idx, impulse, damage)
            | _ -> None)

    applyDamageAndImpulse state.ships.speeds state.ships.health damagesDueToCrashes
    applyDamageAndImpulse state.ships.speeds state.ships.health damagesDueToHits
    applyDamageAndImpulse state.ships.speeds state.ships.health remoteDamages

    let ships = integrateShips dt state.ships description.shipTypes forces headings rights description.localPlayersIdxs
    
    { state with
        ships = ships
        bullets = bullets
        time = state.time + dmsFromS dt
    }
