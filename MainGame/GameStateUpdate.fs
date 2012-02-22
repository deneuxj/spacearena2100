module SpaceArena2100.GameStateUpdate

open Microsoft.Xna.Framework

open CleverRake.XnaUtils
open CleverRake.XnaUtils.Random

open Units
open GameState
open WarpCoord

type ShipPosition =
    { position : TypedVector3<m>
      heading : TypedVector3<1>
      right : TypedVector3<1>
    }

/// Synchronization message types sent over the network
type RemoteEvent =
    | DamageAndImpulse of int<GPI> * float32<Health> * TypedVector3<m/s> // Ship idx, damage, impulse
    | BulletDestroyed of int<BulletGuid> // Bullet GUID
    | ShipState of int<GPI> * ShipPosition * TypedVector3<m/s> * TypedVector3<m/s^2> // Ship idx, position, heading, right, speed, acceleration
    | ShipDestroyed of int<GPI> // Ship idx
    | BulletFired of int<BulletGuid> * int<GPI> * float32<m> * TypedVector3<m> * TypedVector3<m/s> // Bullet GUID, owner, radius, position, speed
    | SupplySpawned of int<GSI> * int<dms> * TypedVector3<m> * float32<m> * SupplyType // Supply idx, life length, position, radius, type
    | SupplyDisappeared of int<GSI> // Supply idx
    | PlayerJoined of int<GPI> * int<LivePlayer> * string * ShipPosition // Ship idx, player name
    | PlayerLeft of int<GPI> // Ship idx

/// A synchronization event with the time at which it was sent.
type TimedRemoteEvent =
    { time : int<dms>;
      event : RemoteEvent }

/// Create new bullets according to BulletFired events
let createBullets guidIsLocal now bulletCount events =
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

    let bulletCount =
        newGuids
        |> Array.fold (fun bulletCount guid -> if guidIsLocal guid then max (countOfBulletGuid guid) bulletCount else bulletCount) bulletCount

    { guids = newGuids;
      owners = newOwners;
      radii = newRadii;
      speeds = newSpeeds;
      timeLeft = newTimeLeft;
      pos = newPos;
      bulletCounter = bulletCount }

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
      bulletCounter = bullets.bulletCounter }


/// Merge two bullet groups
let appendBullets bullets1 bullets2 =
    { guids = Array.append bullets1.guids bullets2.guids;
      owners = Array.append bullets1.owners bullets2.owners;
      radii = Array.append bullets1.radii bullets2.radii;
      speeds = Array.append bullets1.speeds bullets2.speeds;
      timeLeft = Array.append bullets1.timeLeft bullets2.timeLeft;
      pos = Array.append bullets1.pos bullets2.pos
      bulletCounter = max bullets1.bulletCounter bullets2.bulletCounter }


/// Check intersection between a moving sphere and asteroids
let intersectSphereVsAsteroids (dt : float32<s>) (asteroids : Asteroids) (pos : TypedVector3<m>) (speed : TypedVector3<m/s>) (radius : float32<m>) =
    let warp = warp (asteroids.fieldSizes.v)
    let wpos = TypedVector3<m>(warp pos.v)
    let course = dt * speed
    assert (radius > 0.0f<m>)

    let p1 =
        TypedVector3<m>(
            (if speed.X > 0.0f<m/s> then
                wpos.X - radius
             else
                wpos.X + radius),
            (if speed.Y > 0.0f<m/s> then
                wpos.Y - radius
             else
                wpos.Y + radius),
            (if speed.Z > 0.0f<m/s> then
                wpos.Z - radius
             else
                wpos.Z + radius)
        )

    let endPos = wpos + course
    let p2 =
        TypedVector3<m>(
            (if speed.X > 0.0f<m/s> then
                endPos.X + radius
             else
                endPos.X - radius),
            (if speed.Y > 0.0f<m/s> then
                endPos.Y + radius
             else
                endPos.Y - radius),
            (if speed.Z > 0.0f<m/s> then
                endPos.Z + radius
             else
                endPos.Z - radius)
        )

    let travelBox = Octree.mkBBox p1.v p2.v

    let check idxAsteroid =
        let astPos = asteroids.pos.[idxAsteroid]
        if Vector3.Dot(warp (astPos.v - pos.v), speed.v) >= 0.0f then
            match TrajectoryCollision.getIntersectionTime(wpos.v, speed.v, float32 radius, astPos.v, Vector3.Zero, float32 asteroids.radius.[idxAsteroid]) with
            | TrajectoryCollision.IntersectionAt t ->
                let t = t * 1.0f<s>
                if 0.0f<s> <= t && t <= dt then
                    Some idxAsteroid
                else
                    None
            | TrajectoryCollision.NoIntersection -> None
        else
            None

    Octree.checkIntersection
        (fun (bbox : BoundingBox) -> travelBox.Intersects(bbox))
        (ArrayInlined.tryMapFirst check)
        asteroids.octree


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

                for shipIdx in ships.posVisible.First .. 1<GPI> .. ships.posVisible.Last do
                    let pos' = ships.posVisible.[shipIdx]
                    let speed' = ships.speeds.[shipIdx]
                    let shipType : ShipType = MarkedArray.get shipTypes shipIdx
                    let radius' = shipType.BoundingSphereRadius
                    assert (radius' > 0.0f<m>)

                    match TrajectoryCollision.getIntersectionTime (pos.v, speed.v, float32 radius, pos'.v, speed'.v, float32 radius') with
                    | TrajectoryCollision.IntersectionAt t ->
                        let t = 1.0f<s> * t
                        if t >= 0.0f<s> && t < dt then
                            yield (shipIdx, guid, pos, speed, radius)
                    | TrajectoryCollision.NoIntersection -> ()
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
        let bulletMass = K * r3 * BulletConstants.density
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
      bulletCounter = bullets.bulletCounter }

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
        Array.map2
            (fun speed (accel : TypedVector3<m/s^2>) ->
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
            posHost
            posClient
            ships.posLerpT.Content

#if DEBUG
    for shipIdx in localPlayers do
        let shipIdx = int shipIdx
        assert(posClient.[shipIdx] = posHost.[shipIdx])
        assert((posClient.[shipIdx] - posVisible.[shipIdx]).Length < 0.001f<m>)
#endif

    { ships with
        accels = accels
        headings = headings
        rights = rights
        speeds = MarkedArray speeds2
        posClient = MarkedArray posClient
        posHost = MarkedArray posHost
        posVisible = MarkedArray posVisible
    }


let integratePlayers (dt : float32<s>) players =
    let dtInDms = intFromFloat32 (dt * dmsPerS_f)

    { players with
        timeBeforeFire = players.timeBeforeFire |> List.map (fun x -> x - dtInDms)
    }


/// Mutate the health of ships which have been destroyed, setting it to 0.
let destroyShips (events : TimedRemoteEvent[]) (ships : Ships) =
    for e in events do
        match e with
        | { event = ShipDestroyed idx } ->
            ships.health.[idx] <- 0.0f<Health>
        | _ -> ()


/// Mutate posHost and the interpolation parameter posLerpT depending on sync events.
let updateDeadReckoning frameDt (now : int<dms>) (ships : Ships) (events : TimedRemoteEvent[]) =
    let lerpTime = 0.3f<s>
    let updateOfShip =
        events
        |> Array.choose (
            function
            | { time = t; event = ShipState (shipIdx, { position = pos; heading = heading; right = right }, speed, accel) } -> Some (shipIdx, (t, pos, heading, right, speed, accel))
            | _ -> None)
        |> Map.ofSeq

    for shipIdx in ships.headings.First .. 1<GPI> .. ships.headings.Last do
        match Map.tryFind shipIdx updateOfShip with
        | Some (time, pos, heading, right, speed, accel) ->
            // Update posHost, speed and accel by integrating from the timestamp of the update event.
            // The old posHost is moved to posClient.
            ships.posClient.[shipIdx] <- ships.posHost.[shipIdx]
            let dt = int2float32 ((now - time) / dmsPerS)
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


let addNewShipRandom (random : System.Random) ships =
    let pos = TypedVector3<m>(random.NextVector3(100.0f))
    let orientation = random.NextQuaternion()
    let heading = TypedVector3<1>(Vector3.Transform(Vector3.UnitY, orientation))
    let right = TypedVector3<1>(Vector3.Transform(Vector3.UnitX, orientation))

    let ships : Ships =
        { accels = MarkedArray.add (TypedVector3<m/s^2>()) ships.accels
          headings = MarkedArray.add heading ships.headings
          rights = MarkedArray.add right ships.rights
          posHost = MarkedArray.add pos ships.posHost
          posVisible = MarkedArray.add pos ships.posVisible
          posClient = MarkedArray.add pos ships.posClient
          posLerpT = MarkedArray.add 1.0f ships.posLerpT
          speeds = MarkedArray.add (TypedVector3<m/s>()) ships.speeds
          health = MarkedArray.add 1.0f<Health> ships.health
          scores = MarkedArray.add 0.0f<Points> ships.scores
        }

    ships


let addNewShip (position : ShipPosition) ships =
    { accels = MarkedArray.add (TypedVector3<m/s^2>()) ships.accels
      headings = MarkedArray.add position.heading ships.headings
      rights = MarkedArray.add position.right ships.rights
      posHost = MarkedArray.add position.position ships.posHost
      posVisible = MarkedArray.add position.position ships.posVisible
      posClient = MarkedArray.add position.position ships.posClient
      posLerpT = MarkedArray.add 1.0f ships.posLerpT
      speeds = MarkedArray.add (TypedVector3<m/s>()) ships.speeds
      health = MarkedArray.add 1.0f<Health> ships.health
      scores = MarkedArray.add 0.0f<Points> ships.scores
    }


let updateSupplies (dt : int<dms>) (now : int<dms>) (events : TimedRemoteEvent[]) supplies =
    for e in events do
        match e with
        | { time = t0; event = SupplySpawned(idx, timeLeft, pos, radius, supType) } ->
            supplies.timeLeft.[idx] <- timeLeft - (now - t0)
            supplies.pos.[idx] <- pos
            supplies.radii.[idx] <- radius
            supplies.types.[idx] <- supType
        | { event = SupplyDisappeared idx } ->
            supplies.timeLeft.[idx] <- 0<dms>
        | _ -> ()            

    for idx in supplies.timeLeft.First .. 1<GSI> .. supplies.timeLeft.Last do
        let timeLeft = supplies.timeLeft.[idx] - dt
        supplies.timeLeft.[idx] <- timeLeft
        

let diffStates (state : State) (state' : State) =
    let now = state'.time
    let shipStates =
        state'.players.localPlayersIdxs
        |> List.fold (fun messages idx ->
            let shipPos : ShipPosition =
                { position = state'.ships.posHost.[idx]
                  heading = state'.ships.headings.[idx]
                  right = state'.ships.rights.[idx] }
            { time = now
              event = ShipState(idx, shipPos, state'.ships.speeds.[idx], state'.ships.accels.[idx]) }
            :: messages) []

    let shipsDestroyed =
        state.players.localPlayersIdxs
        |> List.fold (fun messages idx ->
            if state.ships.health.[idx] > 0.0f<Health> &&
               state'.ships.health.[idx] <= 0.0f<Health> then
                { time = now
                  event = ShipDestroyed idx }
                :: messages
            else
                messages) []
    
    List.concat
        [shipStates ; shipsDestroyed]


/// Update the game state.
let update dt events forces headings rights (description : Description) (state : State) =
    let guidIsLocal = guidIsLocal state.players.localPlayersIdxs
    
    updateDeadReckoning dt state.time state.ships events

    let bullets =
        createBullets guidIsLocal state.time state.bullets.bulletCounter events
        |> appendBullets state.bullets
    
    let bulletHitsOnShips = computeHits guidIsLocal dt state.ships state.players.shipTypes state.bullets
    let bulletHitsOnAsteroids = computeBulletAsteroidHits guidIsLocal dt description.asteroids state.bullets
    let crashes = computeCrashes dt description.asteroids state.ships state.players.shipTypes state.players.localPlayersIdxs
    
    let damagesDueToHits = computeHitResponse state.ships state.players.shipTypes bulletHitsOnShips
    let damagesDueToCrashes = computeCrashResponse description.asteroids state.ships state.players.shipTypes crashes

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

    let ships = integrateShips dt state.ships state.players.shipTypes forces headings rights state.players.localPlayersIdxs
    destroyShips events ships

    // copy the supplies arrays before mutating
    let supplies =
        { pos = state.supplies.pos.Content |> Array.copy |> MarkedArray
          types = state.supplies.types.Content |> Array.copy |> MarkedArray
          radii = state.supplies.radii.Content |> Array.copy |> MarkedArray
          timeLeft = state.supplies.timeLeft.Content |> Array.copy |> MarkedArray
        }

    updateSupplies (dmsFromS dt) state.time events supplies

    let state' =
        { state with
            players = state.players |> integratePlayers dt
            ships = ships
            bullets = bullets
            time = state.time + dmsFromS dt
            supplies = supplies
        }

    // Messages to send to other participants in an online multiplayer game.
    let messages = diffStates state state'

    let messages =
        Seq.concat [ guidsDestroyedOnAsteroids ; guidsDestroyedOnHits ]
        |> Seq.fold (fun messages guid ->
            { time = state'.time
              event = BulletDestroyed guid }
            :: messages) messages
    
    let messages =
        Seq.concat [ damagesDueToCrashes ; damagesDueToHits ]
        |> Seq.fold (fun messages (idx, impulse, damage) ->
            { time = state'.time
              event = DamageAndImpulse (idx, damage, impulse) }
            :: messages) messages

    // Return updated state and messages to send out
    state'
    ,
    messages

