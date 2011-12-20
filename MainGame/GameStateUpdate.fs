module SpaceArena2100.GameStateUpdate

open Microsoft.Xna.Framework
open CleverRake.XnaUtils

open Units

type RemoteEvent =
    | DamageAndImpulse of int * float32 * Vector3
    | BulletDestroyed of int
    | ShipState of int * Vector3 * Vector3 * Vector3 * Vector3 * Vector3 // Ship idx, position, heading, right, speed, thrust
    | BulletFired of int * int * float32 * Vector3 * Vector3 // Guid, owner, radius, position, speed

type TimedRemoteEvent =
    { time : int;
      event : RemoteEvent }

