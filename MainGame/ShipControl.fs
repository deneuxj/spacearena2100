module SpaceArena2100.ShipControl

open Microsoft.Xna.Framework
open Microsoft.Xna.Framework.Input

open CleverRake.XnaUtils
open CleverRake.XnaUtils.QuaternionExtensions
open CleverRake.XnaUtils.Units

/// Compute the side force to apply to nullify lateral velocity.
let computeSideForce (drag : float32<N s/m>) (right : TypedVector3<1>) (speed : TypedVector3<m/s>) : float32<N> =
    let sideSpeed = TypedVector.dot3 (speed, right)
    (-drag) * sideSpeed

/// Compute the vertical force to apply to nullify vertical speed.
let computeVerticalForce (drag : float32<N s/m>) (heading : TypedVector3<1>) (right : TypedVector3<1>) (speed : TypedVector3<m/s>) : float32<N> =
    let up = TypedVector.cross3 (right, heading)
    let sideSpeed = TypedVector.dot3 (speed, up)
    (-drag) * sideSpeed

/// Compute forward/backward thrust to apply to meet a given target speed.
let computeThrust (breakDrag : float32<N s/m>) (thrustDrag : float32<N s/m>) (heading : TypedVector3<1>) (speed : TypedVector3<m/s>) targetSpeed : float32<N> =
    match TypedVector.dot3 (speed, heading) - targetSpeed with
    | forwardSpeed when forwardSpeed > 0.0f<m/s> -> -breakDrag * forwardSpeed
    | backwardSpeed -> -thrustDrag * backwardSpeed

/// Input units: ranges from -1.0 to 1.0
[<Measure>] type iu

// Extensions to ShipType related to controls.
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

    member this.MaxForwardSpeed =
        600.0f<m/s>

    member this.MaxBackwardSpeed =
        10.0f<m/s>

    member this.TargetSpeedRate =
        200.0f<m/s^2/iu>


/// Personal setting: invert Y
type VerticalAxis = Direct | Inverted

/// Personal setting: which thumbstick steers.
type SteersWith = LeftThumb | RightThumb

/// Personal setting: fire with triggers or bumpers?
type FiresWith = Triggers | Bumpers

/// Personal settings for gamepads.
type GamepadControlSettings =
    { steering : SteersWith;
      pullOrientation : VerticalAxis;
      thrustOrientation : VerticalAxis;
      lookOrientation : VerticalAxis;
      firesWith : FiresWith }

let defaultSettings =
    { steering = LeftThumb;
      pullOrientation = Direct;
      thrustOrientation = Direct;
      lookOrientation = Direct;
      firesWith = Bumpers }
      
/// Controls of a ship. Analog controls are expressed in Input Units.
type Controls =
    { turnRight : float32<iu>
      turnUp : float32<iu>
      forwardSpeedAdjust : float32<iu>
      fireRequested : bool }

/// Extract device-independent orders from the game pad of a given player index and personal settings.
let getControls settings (pi : PlayerIndex) =
    let state = GamePad.GetState(pi)
    let turnRight =
        match settings.steering with
        | LeftThumb -> state.ThumbSticks.Left.X
        | RightThumb -> state.ThumbSticks.Right.X

    let turnUp =
        match settings.steering with
        | LeftThumb -> state.ThumbSticks.Left.Y
        | RightThumb -> state.ThumbSticks.Right.Y
        *
        match settings.pullOrientation with
        | Direct -> 1.0f
        | Inverted -> -1.0f

    let forwardSpeedAdjust =
        match settings.firesWith with
        | Bumpers ->
            (state.Triggers.Right - state.Triggers.Left)
        | Triggers ->
            if state.Buttons.RightShoulder = ButtonState.Pressed then
                1.0f
            else
                0.0f
            -
            if state.Buttons.LeftShoulder = ButtonState.Pressed then
                1.0f
            else
                0.0f

    let fireRequested =
        match settings.firesWith with
        | Bumpers ->
            state.Buttons.RightShoulder = ButtonState.Pressed
        | Triggers ->
            state.Triggers.Right > 0.5f

    let pwr (x : float32) : float32 = x * x * x

    { turnRight = 1.0f<iu> * pwr turnRight
      turnUp = 1.0f<iu> * pwr turnUp
      forwardSpeedAdjust = 1.0f<iu> * forwardSpeedAdjust
      fireRequested = fireRequested }

/// Given inputs from local players, compute new heading, right vector, target speed and jet forces.
let handlePlayerInputs (dt : float32<s>) localPlayers settings playerIndices (ships : GameState.Ships) (shipTypes : MarkedArray<GameState.GPI, GameState.ShipType>) =
    let controls =
        List.map2 getControls settings playerIndices

    let clamp low hi x =
        x
        |> max low
        |> min hi

    let hrt =
        [
            for shipIdx, controls, targetSpeed in List.zip3 localPlayers controls ships.localTargetSpeeds do
                let shipType = shipTypes.[shipIdx]
                let heading = ships.headings.[shipIdx]
                let right = ships.rights.[shipIdx]
                let up = TypedVector.cross3(right, heading)
                
                let turnRate = shipType.TurnRate
                let q = Quaternion.CreateFromAxisAngle(up, -controls.turnRight * dt * turnRate)
                let right = TypedVector3<1>(Vector3.Transform(right.v, q))
                let heading = TypedVector3<1>(Vector3.Transform(heading.v, q))

                let q = Quaternion.CreateFromAxisAngle(right, controls.turnUp * dt * turnRate)
                let heading = TypedVector3<1>(Vector3.Transform(heading.v, q))

                let speedRate = shipType.TargetSpeedRate
                let targetSpeed =
                    (targetSpeed
                     +
                     controls.forwardSpeedAdjust * dt * speedRate)
                    |> clamp -shipType.MaxBackwardSpeed shipType.MaxForwardSpeed
                
                yield (heading, right, targetSpeed)
        ]

    let forces =
        [
            for shipIdx, (heading, right, targetSpeed) in List.zip localPlayers hrt do
                let speed = ships.speeds.[shipIdx]
                let shipType = shipTypes.[shipIdx]
                let forceRight =
                    (computeSideForce shipType.SideDrag right speed
                     |> clamp -shipType.MaxSideForce shipType.MaxSideForce)
                    * TypedVector3<1>(1.0f<1>, 0.0f<1>, 0.0f<1>)
                let forceUp =
                    (computeVerticalForce shipType.VerticalDrag heading right speed
                     |> clamp -shipType.MaxVerticalForce shipType.MaxVerticalForce)
                    * TypedVector3<1>(0.0f<1>, 1.0f<1>, 0.0f<1>)
                let forceForward =
                    (computeThrust shipType.BreakDrag shipType.AccelDrag heading speed targetSpeed
                     |> clamp -shipType.MaxBackwardThrust shipType.MaxForwardThrust)
                    * TypedVector3<1>(0.0f<1>, 0.0f<1>, 1.0f<1>)
                yield forceRight + forceUp + forceForward
        ]

    let headings, rights, targetSpeeds = List.unzip3 hrt

    headings, rights, targetSpeeds, forces
