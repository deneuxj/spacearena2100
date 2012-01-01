module SpaceArena2100.AiSteering

open Microsoft.Xna.Framework

open CleverRake.XnaUtils
open CleverRake.XnaUtils.Units

open SpaceArena2100.GameState
open SpaceArena2100.Units

open SpaceArena2100.ShipControl

let computePathTo delay (ships : Ships) (idxTarget : int<GPI>) (idxAi : int<GPI>) =
    let h t =
        let targetPos = ships.posClient.[idxTarget]
        let targetSpeed = ships.speeds.[idxTarget]
        let futureTargetPos = targetPos + t * targetSpeed

        Hermite.hermite ships.posClient.[idxAi].v ships.speeds.[idxAi].v 0.0f futureTargetPos.v targetSpeed.v (float32 t)

    h delay


let steerFromPath pos speed (heading : TypedVector3<1>) (right : TypedVector3<1>) posFun speedFun =
    let aheadTime = 0.5f
    let aheadPos = TypedVector3<m>(posFun aheadTime)
    let aheadSpeed = TypedVector3<m/s>(speedFun aheadTime)

    let diffPos = aheadPos - pos

    if TypedVector.dot3(diffPos, heading) < 0.0f<m> then
        let turnRight =
            let projX = TypedVector.dot3(diffPos, right)
            if projX > 0.0f<m> then 1.0f else -1.0f
        let longSpeed = TypedVector.dot3(heading, speed)
        let adjustSpeed =
            MathHelper.Clamp(float32 (-longSpeed), -1.0f, 1.0f)

        { turnRight = turnRight * 1.0f<iu>
          turnUp = 0.0f<iu>
          forwardSpeedAdjust = 0.0f<iu>
          fireRequested = false }
    else
        let turnRight =
            let projX = TypedVector.dot3(diffPos, right)
            MathHelper.Clamp(float32 projX, -1.0f, 1.0f)

        let up = TypedVector.cross3(right, heading)
        let turnUp =
            let projY = TypedVector.dot3(diffPos, up)
            MathHelper.Clamp(float32 projY, -1.0f, 1.0f)

        let longSpeed = TypedVector.dot3(heading, speed)
        let adjustSpeed =
            MathHelper.Clamp(float32 (aheadSpeed.Length - longSpeed), -1.0f, 1.0f)

        let ret : Controls =
            { turnRight = turnRight * 1.0f<iu>
              turnUp = turnUp * 1.0f<iu>
              forwardSpeedAdjust = adjustSpeed * 1.0f<iu>
              fireRequested = true }

        ret


let selectTarget (ships : Ships) (idxAi : int<GPI>) =
    let myPos = ships.posClient.[idxAi]

    let paths =
        [|
            for idxTarget in ships.posClient.First .. 1<GPI> .. ships.posClient.Last do
                if idxAi <> idxTarget then
                    let posTarget = ships.posVisible.[idxTarget]
                    let dist = (posTarget - myPos).Length
                    let delay = 1.0f / 10.0f<m/s> * dist
                    match computePathTo delay ships idxTarget idxAi with
                    | Some funs ->
                        yield (idxTarget, funs)
                    | None -> ()
        |]

    if Array.isEmpty paths then
        None
    else
        let idx, (posFun, speedFun, _) =
            paths
            |> Array.minBy(fun (_, (_, _, accelFun)) -> (accelFun 0.0f).Length())
        Some (idx, posFun, speedFun)


let steer ships idxAi =
    match selectTarget ships idxAi with
    | Some (idxTarget, posFun, speedFun) ->
        steerFromPath
            ships.posClient.[idxAi]
            ships.speeds.[idxAi]
            ships.headings.[idxAi]
            ships.rights.[idxAi]
            posFun
            speedFun
    | None ->
        { turnRight = 0.0f<iu>
          turnUp = 0.0f<iu>
          forwardSpeedAdjust = 0.0f<iu>
          fireRequested = false }