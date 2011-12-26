module SpaceArena2100.ShipControl

open Microsoft.Xna.Framework
open Microsoft.Xna.Framework.Input

open CleverRake.XnaUtils
open CleverRake.XnaUtils.QuaternionExtensions
open CleverRake.XnaUtils.Units

let computeSideForce (drag : float32<N s/m>) (right : TypedVector3<1>) (speed : TypedVector3<m/s>) : float32<N> =
    let sideSpeed = TypedVector.dot3 (speed, right)
    (-drag) * sideSpeed

let computeVerticalForce (drag : float32<N s/m>) (heading : TypedVector3<1>) (right : TypedVector3<1>) (speed : TypedVector3<m/s>) : float32<N> =
    let up = TypedVector.cross3 (right, heading)
    let sideSpeed = TypedVector.dot3 (speed, up)
    (-drag) * sideSpeed

let computeThrust (breakDrag : float32<N s/m>) (thrustDrag : float32<N s/m>) (heading : TypedVector3<1>) (speed : TypedVector3<m/s>) targetSpeed : float32<N> =
    match TypedVector.dot3 (speed, heading) - targetSpeed with
    | forwardSpeed when forwardSpeed > 0.0f<m/s> -> -breakDrag * forwardSpeed
    | backwardSpeed -> -thrustDrag * backwardSpeed

/// Input units: ranges from -1.0 to 1.0
[<Measure>] type iu

type GameState.ShipType
with
    member this.SideDrag =
        10.0f<N s/m>

    member this.VerticalDrag =
        10.0f<N s/m>

    member this.AccelDrag =
        20.0f<N s/m>

    member this.BreakDrag =
        10.0f<N s/m>

    member this.TurnRate : float32<rad/s/iu> =
        LanguagePrimitives.Float32WithMeasure(MathHelper.PiOver2)

    member this.MaxSideForce =
        100.0f<N>

    member this.MaxVerticalForce =
        100.0f<N>

    member this.MaxForwardThrust =
        500.0f<N>

    member this.MaxBackwardThrust =
        100.0f<N>

    member this.ForwardMaxSpeed =
        600.0f<m/s>

    member this.MaxBackwardSpeed =
        10.0f<m/s>

    member this.TargetSpeedRate =
        200.0f<m/s^2/iu>


type VerticalAxis = Direct | Inverted

type SteersWith = LeftThumb | RightThumb

type GamepadControlSettings =
    { steering : SteersWith;
      pullOrientation : VerticalAxis
      thrustOrientation : VerticalAxis
      lookOrientation : VerticalAxis }

type Controls =
    { turnRight : float32<iu>
      turnUp : float32<iu>
      forwardSpeedAdjust : float32<iu> }

let getControls settings (pi : PlayerIndex) =
    let state = GamePad.GetState(pi)
    let turnRight =
        1.0f<iu>
        *
        match settings.steering with
        | LeftThumb -> state.ThumbSticks.Left.X
        | RightThumb -> state.ThumbSticks.Right.X

    let turnUp =
        match settings.steering with
        | LeftThumb -> state.ThumbSticks.Left.X
        | RightThumb -> state.ThumbSticks.Right.X
        *
        match settings.pullOrientation with
        | Direct -> 1.0f<iu>
        | Inverted -> -1.0f<iu>

    let forwardSpeedAdjust =
        1.0f<iu>
        *
        (state.Triggers.Right - state.Triggers.Left)

    { turnRight = turnRight
      turnUp = turnUp
      forwardSpeedAdjust = forwardSpeedAdjust }

let handlePlayerInputs (dt : float32<s>) localPlayers settings playerIndices (ships : GameState.Ships) (shipTypes : MarkedArray<GameState.GPI, GameState.ShipType>) =
    let controls =
        List.map2 getControls settings playerIndices

    let headings, rights =
        [
            for shipIdx, controls in List.zip localPlayers controls do
                let heading = ships.headings.[shipIdx]
                let right = ships.rights.[shipIdx]
                let up = TypedVector.cross3(right, heading)
                let turnRate = shipTypes.[shipIdx].TurnRate
                let q = Quaternion.CreateFromAxisAngle(up, -controls.turnRight * dt * turnRate)
                let right = TypedVector3<1>(Vector3.Transform(right.v, q))
                let q = Quaternion.CreateFromAxisAngle(right, controls.turnUp * dt * turnRate)
                let heading = TypedVector3<1>(Vector3.Transform(heading.v, q))

                yield (heading, right)
        ]
        |> List.unzip

    headings, rights