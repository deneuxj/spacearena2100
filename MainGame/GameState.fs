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

let bulletDensity = 1.0f<kg/m^3>

type Asteroids =
    { pos : MarkedArray<AstIdx, TypedVector3<m>>;
      radius : MarkedArray<AstIdx, float32<m>>;
      rotX : MarkedArray<AstIdx, float32<rad>>;
      rotY : MarkedArray<AstIdx, float32<rad>>;
      octree : Octree.Node<int<AstIdx>>;
      fieldSizes : TypedVector3<m> }
    
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

