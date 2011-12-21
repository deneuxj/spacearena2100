module SpaceArena2100.GameStateUpdate

open Microsoft.Xna.Framework
open CleverRake.XnaUtils

open Units
open GameState

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

/// Add entries to arrays describing bullets according to BulletFired events
let createBullets now bulletGuids bulletOwners bulletRadii bulletPos bulletSpeeds events =
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
        let inline mult (a, b) = CleverRake.XnaUtils.Speed3.(*) (a, b)
        events
        |> Array.map (function { time = t0; event = BulletFired(_, _, _, pos, speed) } -> pos + mult(int2float32((now - t0) / (dmsPerS)), speed) | _ -> failwith "Unreachable")

    let newSpeeds =
        events
        |> Array.map (function { event = BulletFired(_, _, _, _, speed) } -> speed | _ -> failwith "Unreachable")

    Array.append bulletGuids newGuids,
    Array.append bulletOwners newOwners,
    Array.append bulletRadii newRadii,
    Array.append bulletSpeeds newSpeeds

/// Mutate speed and array events as requested by DamageAndImpulse entries in a list of events.
let applyDamageAndImpulse speeds health events =
    for e in events do
        match e with
        | { event = DamageAndImpulse (idx, damage, impulse) } ->
            MarkedArray.mutate ((+) impulse) (speeds, idx)
            MarkedArray.mutate (fun x -> x - damage) (health, idx)
        | _ -> ()

/// Remove entries from arrays describing bullets as requested by BulletDestroyed entries in a list of events.
let destroyBullets bulletGuids bulletOwners bulletRadii bulletSpeeds events =
    let guidsToRetire =
        events
        |> Array.choose(function { event = BulletDestroyed guid } -> Some guid | _ -> None)
        |> Set.ofArray

    let newOwners =
        bulletOwners
        |> ArrayInlined.filterRef (guidsToRetire.Contains >> not) bulletGuids

    let newRadii =
        bulletRadii
        |> ArrayInlined.filterRef (guidsToRetire.Contains >> not) bulletGuids

    let newSpeeds =
        bulletSpeeds
        |> ArrayInlined.filterRef (guidsToRetire.Contains >> not) bulletGuids

    let newGuids =
        bulletGuids
        |> Array.filter (guidsToRetire.Contains >> not)

    newGuids, newOwners, newRadii, newSpeeds