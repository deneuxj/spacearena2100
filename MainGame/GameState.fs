﻿module SpaceArena2100.GameState

open Microsoft.Xna.Framework
open CleverRake.XnaUtils
open Units

/// Kinds of re-supplies.
type SupplyType =
    | FastBullets
    | BigBullets
    | MultiFire
    | HighRate

/// Kinds of ships
type ShipType =
    | Bull
    | Hawk
    | Sphere
with
    member this.BoundingSphereRadius =
        match this with
        | _ -> 3.0f<m>

/// Global player index
[<Measure>] type GPI

/// Asteroid index
[<Measure>] type AstIdx

/// Global supply index
[<Measure>] type GSI

/// Globally unique id of bullets
[<Measure>] type BulletGuid

/// Angles, radians
[<Measure>] type rad

type Asteroids =
    { pos : MarkedArray<AstIdx, TypedVector3<m>>;
      radius : MarkedArray<AstIdx, float32<m>>;
      rotX : MarkedArray<AstIdx, float32<rad>>;
      rotY : MarkedArray<AstIdx, float32<rad>>;
      octree : Octree.Node<int<AstIdx>> }
    
/// Data that does not change often during a round.
type Description =
    { numPlayers : int;
      myHostId : int;
      areaSize : float32<m>;
      playerNames : string[];
      localPlayersIdxs : int<GPI> list;
      localAiPlayerIdxs : int<GPI> list;
      remotePlayerIdxs : int<GPI> list;
      shipTypes : ShipType[];
      /// Players who left the game early.
      gonePlayerIdxs : int<GPI> list;
      asteroids : Asteroids;
    }

type Ships =
    { headings : MarkedArray<GPI, TypedVector3<m>>;
      rights : MarkedArray<GPI, TypedVector3<m>>;
      /// Position we had computed last time we received a position for the player's host.
      posClient : MarkedArray<GPI, TypedVector3<m>>;
      /// Position where players here see this ship. Interpolated between posClient and posHost.
      posVisible : MarkedArray<GPI, TypedVector3<m>>;
      /// Last known position from the host of the player.
      posHost : MarkedArray<GPI, TypedVector3<m>>;
      /// Time of last known position received from the host of the player.
      posHostTime : MarkedArray<GPI, int<dms>>;
      /// [0, 1], used to compute posVisible.
      posLerpT : MarkedArray<GPI, float32>;
      speeds : MarkedArray<GPI, TypedVector3<m/s>>;
      accels : MarkedArray<GPI, TypedVector3<m/s^2>>;
      health : MarkedArray<GPI, float32<Health>>;
      numFastBullets : MarkedArray<GPI, int>;
      numBigBullets : MarkedArray<GPI, int>;
      numMultiFire : MarkedArray<GPI, int>;
      numHighRate : MarkedArray<GPI, int>;
      timeBeforeFire : MarkedArray<GPI, int<dms>>;
      /// -1 means "not dead".
      timeBeforeRespawn : MarkedArray<GPI, int<dms>>;
      scores : MarkedArray<GPI, float32<Points>> }

type Bullets =
    { guids : int<BulletGuid>[];
      pos : TypedVector3<m>[];
      timeLeft : int<dms>[];
      speeds : TypedVector3<m/s>[];
      radii : float32<m>[];
      owners : int<GPI>[] }

type Supplies =
    { pos : MarkedArray<GSI, Vector3>;
      types : MarkedArray<GSI, SupplyType>;
      radii : MarkedArray<GSI, float32<m>> }

/// Data that changes every frame.
type State =
    { ships : Ships;
      bullets : Bullets;
      supplies : Supplies;      
      time : int<dms>;
    }


(*
let update checkCollisionsWithAsteroids checkCollisionsWithBullets checkCollisionsBetweenShips checkCollisionsWithSupplies computeCollisionImpulses computeDamages applySupplies (description : Description) (state : State) =
    let myPlayers = 
        description.localPlayersIdxs
        |> Set.ofList
        |> Set.union (description.localAiPlayerIdxs |> Set.ofList)

    let myPlayersIdxToAllIdx =
        myPlayers
        |> Set.toArray

    let inline isMyPlayer i _ = myPlayers.Contains i

    let myPlayerPos =
        state.posHost
        |> ArrayInlined.filteri isMyPlayer

    let myPlayerSpeeds =
        state.speed
        |> ArrayInlined.filteri isMyPlayer

    let myPlayerShipTypes =
        description.shipTypes
        |> ArrayInlined.filteri isMyPlayer

    let collisionsWithBullets : ShipCollision list = checkCollisionsWithBullets state.posVisible state.speed description.shipTypes state.bulletPos state.bulletSpeed state.bulletRadius 

    let collisionsWithAsteroids : ShipCollision list = checkCollisionsWithAsteroids myPlayerPos myPlayerSpeeds myPlayerShipTypes description.asteroidPos description.asteroidRadius description.asteroidOctree

    let collisionsBetweenShips : ShipCollision list = checkCollisionsBetweenShips myPlayerPos myPlayerSpeeds myPlayerShipTypes state.posVisible state.speed

    let collisionsWithSupplies : ShipCollision list = checkCollisionsWithSupplies myPlayerPos myPlayerSpeeds myPlayerShipTypes state.supplyPos state.supplyType

    failwith "TODO" *)
(* Update cycle:
 * Check collisions
 * Compute collision impulses
 * Damages to ships due to collisions
 * Respawn ships
 * Retire bullets
 * Apply supplies that were caught
 * Retire supplies
 * Integrate object movement
 * Camera shaking
 *)