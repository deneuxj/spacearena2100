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

/// Local player index
[<Measure>] type LPI

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
    { heading : Vector3[];
      right : Vector3[];
      /// Position we had computed last time we received a position for the player's host.
      posClient : Vector3[];
      /// Position where players here see this ship. Interpolated between posClient and posHost.
      posVisible : Vector3[];
      /// Last known position from the host of the player.
      posHost : Vector3[];
      /// Time of last known position received from the host of the player.
      posHostTime : int<dms>[];
      /// [0, 1], used to compute posVisible.
      posLerpT : float32[];
      speed : Vector3[];
      accel : Vector3[];
      health : float32<Health>[];
      numFastBullets : int[];
      numBigBullets : int[];
      numMultiFire : int[];
      numHighRate : int[];
      timeBeforeFire : int<dms>[];
      /// NaN means "not dead".
      timeBeforeRespawn : int<dms>[];
      score : float32<Points>[];
      bulletGuid : int<BulletGuid>[];
      bulletPos : Vector3[];
      bulletTimeLeft : int<dms>[];
      bulletSpeed : Vector3[];
      bulletRadius : float32<m>[];
      bulletOwner : int<GPI>[];
      supplyPos : Vector3[];
      supplyType : SupplyType[];
      supplyRadius : float32<m>[];
      time : int<dms>;
    }

/// Synchronization message types sent over the network
type GameEvent =
    | ShipSuicided of int<GPI> // dead ship id
    | ShipKilled of int<GPI> * int<BulletGuid> // killed ship id, bullet guid
    | NewBullet of int * Vector3 * Vector3 * float32<m> // bullet guid, position, speed, radius
    | BulletRetired of int<BulletGuid> // bullet guid
    | PlayerLeft of int<GPI> // Player idx
    | PlayerJoined of int<GPI> * string // Player idx, player name
    | Impulse of int<GPI> * Vector3 // Player idx, impulse vector
    | Damage of int<GPI> * float32<Health> // Player idx, damage amount

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