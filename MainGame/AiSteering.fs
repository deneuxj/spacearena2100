module SpaceArena2100.AiSteering

open Microsoft.Xna.Framework

open CleverRake.XnaUtils
open CleverRake.XnaUtils.Units

open SpaceArena2100.GameState
open SpaceArena2100.Units

open SpaceArena2100.ShipControl

module private Utils =
    let nopControls : Controls =
        { turnRight = 0.0f<iu>
          turnUp = 0.0f<iu>
          forwardSpeedAdjust = 0.0f<iu>
          fireRequested = false }


    let clampOne x = MathHelper.Clamp(x, -1.0f, 1.0f)

open Utils

let trackingTime = 5.0f<s>
let shootingTime = 5.0f<s>
let steeringTime = 0.1f<s>


let getAccels (ships : Ships) (shipTypes : MarkedArray<GPI, GameState.ShipType>) (idx : int<GPI>) =
    let heading = ships.headings.[idx]
    let right = ships.rights.[idx]
    let up = TypedVector.cross3(right, heading)

    let myShipType = shipTypes.[idx]

    let maxForwardAccel = myShipType.MaxForwardThrust * myShipType.InversedMass
    let maxBackwardAccel = myShipType.MaxBackwardThrust * myShipType.InversedMass

    let forward =  maxForwardAccel * heading
    let backward = -maxBackwardAccel * heading
    let right = maxForwardAccel * right
    let up = maxForwardAccel * up

    [|
        TypedVector3<m/s^2>()
        forward
        forward + right
        forward - right
        forward + up
        forward - up
        right
        -1.0f * right
        up
        -1.0f * up
        backward
    |]


let evalDistance (d : float32<m>) =
    let d = float32 d
    1.0f / (1.0f + log (1.0f + d))


let evalSituation (dt : float32<s>) (ships : Ships) (shipTypes : MarkedArray<GPI, GameState.ShipType>) (idxAi : int<GPI>) (pos : TypedVector3<m>) (speed : TypedVector3<m/s>) =
    let evalOneShip (idxOther : int<GPI>) (accelOther : TypedVector3<m/s^2>) : float32 =
        let speedOther = ships.speeds.[idxOther] + dt * accelOther
        let posOther = ships.posHost.[idxOther] + dt * speedOther
        let distance = (posOther - pos).Length
        evalDistance distance

(*        -
        (1.0f - evalDistance (1e-4f * dt * (speedOther - speed).Length))
*)

    let zeroAccel = TypedVector3<m/s^2>()
    let vals =
        [|
            for idx in ships.posClient.First .. 1<GPI> .. ships.posClient.Last do
                if idx <> idxAi && ships.health.[idx] > 0.0f<Health> then
                    yield
                        evalOneShip idx zeroAccel
        |]

    match Array.length vals with
    | 0 -> 0.0f
    | n -> Array.sum vals / (float32 n)


let computeBestAccel (dt : float32<s>) (ships : Ships) (shipTypes : MarkedArray<GPI, GameState.ShipType>) (idx : int<GPI>) =
    let evalNewPos (accel : TypedVector3<m/s^2>) =
        let newSpeed = ships.speeds.[idx] + dt * accel
        let newPos = ships.posHost.[idx] + dt * newSpeed
        evalSituation dt ships shipTypes idx newPos newSpeed

    let accelValues =
        getAccels ships shipTypes idx
        |> Array.map (fun accel -> accel, evalNewPos accel)

    let bestAccel = 
        accelValues
        |> Array.maxBy snd
        |> fst

    let bestIdx =
        getAccels ships shipTypes idx
        |> Array.findIndex ((=) bestAccel)

    printfn "%d" bestIdx

    bestAccel


let computePathTo (ships : Ships) (idxTarget : int<GPI>) (idxAi : int<GPI>) =
    let h t =
        let targetPos = ships.posClient.[idxTarget]
        let targetSpeed = ships.speeds.[idxTarget]
        let targetDirection =
            if targetSpeed.Length > 0.01f<m/s> then
                TypedVector.normalize3 targetSpeed
            else
                TypedVector3<1>(Vector3.UnitX)
        let goalPos = targetPos + (max 0.0f<s> (t - 0.2f<s>)) * targetSpeed

        Hermite.hermite ships.posClient.[idxAi].v ships.speeds.[idxAi].v 0.0f goalPos.v targetSpeed.v (float32 t)

    let speedToTarget = 10.0f<m/s>
    let timeToTarget = (ships.posClient.[idxAi] - ships.posClient.[idxTarget]).Length / speedToTarget

    h timeToTarget


let steerFromPath pos speed (heading : TypedVector3<1>) (right : TypedVector3<1>) posFun speedFun accelFun =
    let up = TypedVector.cross3(right, heading)
    let accelGoal = TypedVector3<m/s^2>(accelFun 0.0f)


    let turnRight, turnUp =
        let dir : Vector3 = accelGoal.v

        let ldir = dir.Length()
        if ldir > 0.001f then
            if Vector3.Dot(dir, heading.v) < 0.1f then
                let dir = TypedVector.normalize3(TypedVector3<m>(posFun 1.0f) - pos)
                TypedVector.dot3(dir, right) |> clampOne
                ,
                TypedVector.dot3(dir, up) |> clampOne
            else
                let dir = TypedVector3<1>(dir / ldir)
                TypedVector.dot3(dir, right) |> clampOne
                ,
                TypedVector.dot3(dir, up) |> clampOne
        else
            0.0f, 0.0f

    let magic = 10.0f<m/s^2>
    let adjustSpeed =
        (TypedVector.dot3(accelGoal, heading) / magic)
        |> clampOne

    let adjustSpeed =
        if adjustSpeed < 0.0f then
            -1.0f
        else adjustSpeed

    let ret : Controls =
        { turnRight = turnRight * 1.0f<iu>
          turnUp = turnUp * 1.0f<iu>
          forwardSpeedAdjust = adjustSpeed * 1.0f<iu>
          fireRequested = false }

    ret


let aimAtTarget (bulletSpeed : float32<m/s>) (pos : TypedVector3<m>) (heading : TypedVector3<1>) (right : TypedVector3<1>) (targetPos : TypedVector3<m>) (targetSpeed : TypedVector3<m/s>) =
    let distanceToTarget = (pos - targetPos).Length
    let timeToTarget = distanceToTarget / bulletSpeed
    let goalPos = targetPos + timeToTarget * targetSpeed

    match TypedVector.tryNormalize3 (goalPos - pos) with
    | Some dir ->
        if distanceToTarget < 100.0f<m> && TypedVector.dot3(dir, heading) > 0.9995f then
            let turnRight = TypedVector.dot3(dir, right) |> clampOne
            let up = TypedVector.cross3(right, heading)
            let turnUp = TypedVector.dot3(dir, up) |> clampOne
            let fire = turnRight * turnRight + turnUp * turnUp < 0.05f
            { turnRight = turnRight * 1.0f<iu>
              turnUp = turnUp * 1.0f<iu>
              forwardSpeedAdjust = 0.0f<iu>
              fireRequested = fire }
            |> Some
        else
            None
    | None -> None


let getPathToTarget ships idxAi idxTarget =
    let myPos = ships.posClient.[idxAi]
    let posTarget = ships.posVisible.[idxTarget]
    let dist = (posTarget - myPos).Length
    match computePathTo ships idxTarget idxAi with
    | Some funs ->
        Some (idxTarget, funs)
    | None ->
        None
        
            
let selectTarget (ships : Ships) (idxAi : int<GPI>) =
    let paths =
        [|
            for idxTarget in ships.posClient.First .. 1<GPI> .. ships.posClient.Last do
                if idxAi <> idxTarget then
                    match getPathToTarget ships idxAi idxTarget with
                    | Some x -> yield x
                    | None -> ()
        |]

    if Array.isEmpty paths then
        None
    else
        let idx, (posFun, speedFun, accelFun) =
            paths
            |> Array.minBy(fun (_, (_, _, accelFun)) -> (accelFun 0.0f).Length())
        Some (idx, posFun, speedFun, accelFun)


let steerFromAccel (maxForwardAccel : float32<m/s^2>) (maxBackwardAccel  : float32<m/s^2>) (heading : TypedVector3<1>) (right : TypedVector3<1>) (accelGoal : TypedVector3<m/s^2>) =
    let up = TypedVector.cross3(right, heading)

    let turnRight, turnUp =
        let dir = accelGoal

        match TypedVector.tryNormalize3 accelGoal with
        | Some dir ->
            TypedVector.dot3(dir, right) |> clampOne
            ,
            TypedVector.dot3(dir, up) |> clampOne
        | None ->
            0.0f, 0.0f

    let sq x = x * x
    let correctLength = sq turnRight + sq turnUp

    let requestedAccel =
        if correctLength < 0.01f then
            TypedVector.dot3(accelGoal, heading)
        else
            0.0f<m/s^2>
    
    let adjustSpeed =
        if requestedAccel < 0.0f<m/s^2> then
            requestedAccel / maxBackwardAccel
        else
            requestedAccel / maxForwardAccel
        |> clampOne

    let ret : Controls =
        { turnRight = turnRight * 1.0f<iu>
          turnUp = turnUp * 1.0f<iu>
          forwardSpeedAdjust = adjustSpeed * 1.0f<iu>
          fireRequested = false }

    ret


let updateAi getBulletSpeed (dt : float32<s>) gameState description (state : AiState) idxAi =
    let ships = gameState.ships
    let shipTypes = description.shipTypes

    match state with
    | Undecided ->
        Tactical, nopControls
(*
        match selectTarget ships idxAi with
        | Some (idxTarget, posFun, speedFun, accelFun) ->
            Tracking (trackingTime, idxTarget)
            ,
            nopControls
        | None ->
            Undecided, nopControls
*)

    | Tracking (timeLeft, idxTarget) ->
        let timeLeft = timeLeft - dt
        let nextState =
            if timeLeft > 0.0f<s> then
                let aimCommand =
                    aimAtTarget
                        (getBulletSpeed idxAi)
                        ships.posClient.[idxAi]
                        ships.headings.[idxAi]
                        ships.rights.[idxAi]
                        ships.posVisible.[idxTarget]
                        ships.speeds.[idxTarget]
                match aimCommand with
                | None -> Tracking (timeLeft, idxTarget)
                | Some _ -> ShootingAt (shootingTime, idxTarget)
            else
                Undecided
        let controls =

            match getPathToTarget ships idxAi idxTarget with
            | Some (_, (posFun, speedFun, accelFun)) ->
                steerFromPath
                    ships.posClient.[idxAi]
                    ships.speeds.[idxAi]
                    ships.headings.[idxAi]
                    ships.rights.[idxAi]
                    posFun
                    speedFun
                    accelFun
            | None ->
                nopControls

        nextState, controls
    
    | ShootingAt (timeLeft, idxTarget) ->
        let timeLeft = timeLeft - dt
        let aimCommand =
            aimAtTarget
                (getBulletSpeed idxAi)
                ships.posClient.[idxAi]
                ships.headings.[idxAi]
                ships.rights.[idxAi]
                ships.posVisible.[idxTarget]
                ships.speeds.[idxTarget]
        let nextState =
            if timeLeft > 0.0f<s> && Option.isSome aimCommand then
                ShootingAt (timeLeft, idxTarget)
            else
                Undecided
            
        let controls =
            match aimCommand with
            | None -> nopControls
            | Some x -> x

        nextState, controls

    | Tactical ->
        let bestAccel = computeBestAccel steeringTime ships shipTypes idxAi
        Steering (steeringTime, bestAccel), nopControls

    | Steering (timeLeft, accel) ->
        let timeLeft = timeLeft - dt
        let nextState =
            if timeLeft > 0.0f<s> then
                Steering (timeLeft, accel)
            else Undecided
        let controls =
            let invMass = shipTypes.[idxAi].InversedMass
            steerFromAccel (invMass * shipTypes.[idxAi].MaxBackwardThrust) (invMass * shipTypes.[idxAi].MaxForwardThrust) ships.headings.[idxAi] ships.rights.[idxAi] accel

        nextState, controls
