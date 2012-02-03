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
with
    member this.BoundingSphereRadius =
        match this with
        | _ -> 3.0f<m>
    
    member this.Fragility =
        match this with
        | _ -> 1.0f<Health/(m/s)>

    member this.CollisionRestitution = 0.1f

    member this.InversedMass =
        1.0f / 1000.0f<kg> 

    member this.ToInt() =
        match this with
        | Bull -> 0
        | Hawk -> 1
        | Sphere -> 2

    static member FromInt(x) =
        match x with
        | 0 -> Bull
        | 1 -> Hawk
        | _ -> Sphere

/// Global player index
[<Measure>] type GPI

/// Asteroid index
[<Measure>] type AstIdx

/// Global supply index
[<Measure>] type GSI

/// Globally unique id of bullets
[<Measure>] type BulletGuid

type Asteroids =
    { pos : MarkedArray<AstIdx, TypedVector3<m>>;
      radius : MarkedArray<AstIdx, float32<m>>;
      rotations : MarkedArray<AstIdx, Quaternion>;
      octree : Octree.Node<int<AstIdx> array>;
      fieldSizes : TypedVector3<m> }
    
/// Data that does not change often during a round.
type Description =
    { numPlayers : int;
      myHostId : int;
      playerNames : MarkedArray<GPI, string>;
      localPlayersIdxs : int<GPI> list;
      localAiPlayerIdxs : int<GPI> list;
      shipTypes : MarkedArray<GPI, ShipType>;
      /// Players who left the game early.
      gonePlayerIdxs : int<GPI> list;
      asteroids : Asteroids;
    }

type Ships =
    { headings : MarkedArray<GPI, TypedVector3<1>>;
      rights : MarkedArray<GPI, TypedVector3<1>>;
      /// Position we had computed last time we received a position for the player's host.
      posClient : MarkedArray<GPI, TypedVector3<m>>;
      /// Position where players here see this ship. Interpolated between posClient and posHost.
      posVisible : MarkedArray<GPI, TypedVector3<m>>;
      /// Last known position from the host of the player.
      posHost : MarkedArray<GPI, TypedVector3<m>>;
      /// [0, 1], used to compute posVisible.
      posLerpT : MarkedArray<GPI, float32>;
      speeds : MarkedArray<GPI, TypedVector3<m/s>>;
      accels : MarkedArray<GPI, TypedVector3<m/s^2>>;
      health : MarkedArray<GPI, float32<Health>>;
      // Local players only
      numFastBullets : int list;
      numBigBullets : int list;
      numMultiFire : int list;
      numHighRate : int list;
      timeBeforeFire : int<dms> list;
      /// -1 means "not dead".
      timeBeforeRespawn : int<dms> list;
      localTargetSpeeds : float32<m/s> list;
      scores : MarkedArray<GPI, float32<Points>> }

type Bullets =
    { guids : int<BulletGuid>[];
      pos : TypedVector3<m>[];
      timeLeft : int<dms>[];
      speeds : TypedVector3<m/s>[];
      radii : float32<m>[];
      owners : int<GPI>[];
      lastLocalGuid : int<BulletGuid> }

module BulletConstants =
    let speed = 300.0f<m/s>
    let fastSpeed = 500.0f<m/s>
    let lifetime = 2.0f<s>
    let radius = 0.2f<m>
    let bigRadius = 0.3f<m>
    let firePeriod = 0.25f<s>
    let highRateFirePeriod = 0.1f<s>
    let multiFireSpread = MathHelper.ToRadians(5.0f) * 1.0f<rad>
    let density = 1000.0f<kg/m^3>

/// Check if a bullet guid was produced by a specific host.
let guidIsLocal hostNumber (guid : int<BulletGuid>) =
    (int guid &&& 0xFF) = hostNumber

/// Get the next bullet guid number.
let nextGuid last =
    last + 256<BulletGuid>

/// Get the first bullet guid of a given host.
let firstGuid hostNumber = 1<BulletGuid> * hostNumber

type Supplies =
    { pos : MarkedArray<GSI, Vector3>;
      types : MarkedArray<GSI, SupplyType>;
      radii : MarkedArray<GSI, float32<m>> }

type AiState =
    | Undecided
    | Tracking of float32<s> * int<GPI>
    | ShootingAt of float32<s> * int<GPI>
    | Tactical
    | Steering of float32<s> * TypedVector3<m/s^2>

/// Data that changes every frame.
type State =
    { ships : Ships;
      ais : AiState list;
      bullets : Bullets;
      supplies : Supplies;      
      time : int<dms>;
    }

let emptyState hostId =
    let ships =
        { headings = MarkedArray [||]
          rights = MarkedArray [||]
          posClient = MarkedArray [||]
          posVisible = MarkedArray [||]
          posHost = MarkedArray [||]
          posLerpT = MarkedArray [||]
          speeds = MarkedArray [||]
          accels = MarkedArray [||]
          health = MarkedArray [||]
          numFastBullets = []
          numBigBullets = []
          numMultiFire = []
          numHighRate = []
          timeBeforeFire = []
          timeBeforeRespawn = []
          localTargetSpeeds = []
          scores = MarkedArray [||]
        }

    let bullets =
        { guids = [||]
          pos = [||]
          timeLeft = [||]
          speeds = [||]
          radii = [||]
          owners = [||]
          lastLocalGuid = firstGuid hostId
        }

    let supplies =
        { pos = MarkedArray [||]
          types = MarkedArray [||]
          radii = MarkedArray [||] }

    { ships = ships
      bullets = bullets
      ais = []
      supplies = supplies
      time = 0<dms>
    }

let getBulletSpeed (localPlayers : int<GPI> list) (ships : Ships) (player : int<GPI>) =
    match List.tryFindIndex ((=) player) localPlayers with
    | Some idx ->
        if ships.numFastBullets.[idx] > 0 then
            BulletConstants.fastSpeed
        else
            BulletConstants.speed
    | None ->
        failwith "Cannot get bullet speed of remote player"
