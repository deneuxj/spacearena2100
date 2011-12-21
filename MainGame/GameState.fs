module SpaceArena2100.GameState

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

/// Global player index
[<Measure>] type GPI

/// Global bullet index
[<Measure>] type GBI

/// Global supply index
[<Measure>] type GSI

/// Globally unique id of bullets
[<Measure>] type BulletGuid

/// Angles, radians
[<Measure>] type rad

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
      bulletIsLocal : bool list;
      /// Players who left the game early.
      gonePlayerIdxs : int<GPI> list;
      asteroidPos : Vector3[];
      asteroidRadius : float32<m>[];
      asteroidRotX : float32<rad>[];
      asteroidRotY : float32<rad>[];
      asteroidOctree : Octree.Node<int>;
    }

/// Data that changes every frame.
type State =
    { // *** Ships ***
      heading : MarkedArray<GPI, Vector3>;
      right : MarkedArray<GPI, Vector3>;
      /// Position we had computed last time we received a position for the player's host.
      posClient : MarkedArray<GPI, Vector3>;
      /// Position where players here see this ship. Interpolated between posClient and posHost.
      posVisible : MarkedArray<GPI, Vector3>;
      /// Last known position from the host of the player.
      posHost : MarkedArray<GPI, Vector3>;
      /// Time of last known position received from the host of the player.
      posHostTime : MarkedArray<GPI, int<dms>>;
      /// [0, 1], used to compute posVisible.
      posLerpT : MarkedArray<GPI, float32>;
      speed : MarkedArray<GPI, Vector3>;
      accel : MarkedArray<GPI, Vector3>;
      health : MarkedArray<GPI, float32<Health>>;
      numFastBullets : MarkedArray<GPI, int>;
      numBigBullets : MarkedArray<GPI, int>;
      numMultiFire : MarkedArray<GPI, int>;
      numHighRate : MarkedArray<GPI, int>;
      timeBeforeFire : MarkedArray<GPI, int<dms>>;
      /// -1 means "not dead".
      timeBeforeRespawn : MarkedArray<GPI, int<dms>>;
      score : MarkedArray<GPI, float32<Points>>;

      // *** Bullets ***
      bulletGuid : int<BulletGuid>[];
      bulletPos : MarkedArray<GBI, TypedVector3<m>>;
      bulletTimeLeft : MarkedArray<GBI, int<dms>>;
      bulletSpeed : MarkedArray<GBI, TypedVector3<m>>;
      bulletRadius : MarkedArray<GBI, float32<m>>;
      bulletOwner : MarkedArray<GBI, int<GPI>>;
      
      // *** Supplies ***
      supplyPos : MarkedArray<GSI, Vector3>;
      supplyType : MarkedArray<GSI, SupplyType>;
      supplyRadius : MarkedArray<GSI, float32<m>>;
      
      // *** Other ***
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