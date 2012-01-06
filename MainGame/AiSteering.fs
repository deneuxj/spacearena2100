module SpaceArena2100.AiSteering

open Microsoft.Xna.Framework

open CleverRake.XnaUtils
open CleverRake.XnaUtils.Units

open SpaceArena2100.GameState
open SpaceArena2100.Units

open SpaceArena2100.ShipControl

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

    let candidates =
        //[| 0.1f<s>; 1.0f<s> ; 5.0f<s> |]
        //[| 1.0f<s> |]
        [| (ships.posClient.[idxAi] - ships.posClient.[idxTarget]).Length / 10.0f<m/s> |]
        |> Array.choose (
            fun t ->
                match h t with
                | Some x -> Some (t, x)
                | None -> None)

    let eval (t : float32<s>) (accelFun : float32 -> Vector3) =
        let dt = t / 100.0f
        seq {
            for t in 0.0f<s> .. dt .. t do
                yield (accelFun (float32 t)).Length()
        }
        |> Seq.sum
        |> ((*) (1.0f / t))
        |> float32

    if Array.isEmpty candidates then
        None
    else
        candidates
        |> Array.tryFind(fun (t, (_, _, accelFun)) -> true || eval t accelFun < 10000.0f)
        |> Option.map snd


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
              fireRequested = false }

        ret


let steerFromPath2 pos speed (heading : TypedVector3<1>) (right : TypedVector3<1>) posFun speedFun accelFun =
    let up = TypedVector.cross3(right, heading)
    let accelGoal = TypedVector3<m/s^2>(accelFun 0.0f)

    let clampOne x = MathHelper.Clamp(x, -1.0f, 1.0f)

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
                    match computePathTo ships idxTarget idxAi with
                    | Some funs ->
                        yield (idxTarget, funs)
                    | None -> ()
        |]

    if Array.isEmpty paths then
        None
    else
        let idx, (posFun, speedFun, accelFun) =
            paths
            |> Array.minBy(fun (_, (_, _, accelFun)) -> (accelFun 0.0f).Length())
        Some (idx, posFun, speedFun, accelFun)


let steer ships idxAi =
    match selectTarget ships idxAi with
    | Some (idxTarget, posFun, speedFun, accelFun) ->
        steerFromPath2
            ships.posClient.[idxAi]
            ships.speeds.[idxAi]
            ships.headings.[idxAi]
            ships.rights.[idxAi]
            posFun
            speedFun
            accelFun
    | None ->
        { turnRight = 0.0f<iu>
          turnUp = 0.0f<iu>
          forwardSpeedAdjust = 0.0f<iu>
          fireRequested = false }