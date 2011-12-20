module SpaceArena2100.GameState

open Microsoft.Xna.Framework
open CleverRake.XnaUtils

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

/// Data that does not change often during a round.
type Description =
    { numPlayers : int;
      myHostId : int;
      areaSize : float32;
      playerNames : string[];
      localPlayersIdxs : int list;
      localAiPlayerIdxs : int list;
      remotePlayerIdxs : int list;
      shipTypes : ShipType[];
      bulletIsLocal : bool list;
      /// Players who left the game early.
      gonePlayerIdxs : int list;
      asteroidPos : Vector3[];
      asteroidRadius : float32[];
      asteroidRotX : float32[];
      asteroidRotY : float32[];
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
      posHostTime : int[];
      /// [0, 1], used to compute posVisible.
      posLerpT : float32[];
      speed : Vector3[];
      health : float32[];
      numFastBullets : int[];
      numBigBullets : int[];
      numMultiFire : int[];
      numHighRate : int[];
      timeBeforeFire : int[];
      /// NaN means "not dead".
      timeBeforeRespawn : int[];
      score : float32[];
      bulletGuid : int[];
      bulletPos : Vector3[];
      bulletTimeLeft : int[];
      bulletSpeed : Vector3[];
      bulletRadius : float32[];
      bulletOwner : int[];
      supplyPos : float32[];
      supplyType : SupplyType[];
      supplyRadius : float32[];
      time : int;
    }

type GameEvent =
    | ShipSuicided of int // dead ship id
    | ShipKilled of int * int // killed ship id, bullet guid
    | NewBullet of int * Vector3 * Vector3 * float32 // bullet guid, position, speed, radius
    | BulletRetired of int // bullet guid
    | PlayerLeft of int // Player idx
    | PlayerJoined of int * string // Player idx, player name
    | Impulse of int * Vector3 // Player idx, impulse vector
    | Damage of int * float32 // Player idx, damage amount

type ShipCollision =
    { other : int
      ship : int }

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

    failwith "TODO"
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