module SpaceArena2100.ShipControl

open Microsoft.Xna.Framework
open Microsoft.Xna.Framework.Input

open CleverRake.XnaUtils
open CleverRake.XnaUtils.QuaternionExtensions
open CleverRake.XnaUtils.Units

open SpaceArena2100.Units
open SpaceArena2100.GameState
open SpaceArena2100.GameStateUpdate

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
    | relativeSpeed when relativeSpeed > 0.1f<m/s> -> -breakDrag * relativeSpeed
    | relativeSpeed when relativeSpeed < -0.1f<m/s> -> -thrustDrag * relativeSpeed
    | _ -> 0.0f<N>

/// Input units: ranges from -1.0 to 1.0
[<Measure>] type iu

// Extensions to ShipType related to controls.
type GameState.ShipType
with
    member this.SideDrag =
        5000.0f<N s/m>

    member this.VerticalDrag =
        5000.0f<N s/m>

    member this.AccelDrag =
        5000.0f<N s/m>

    member this.BreakDrag =
        5000.0f<N s/m>

    member this.TurnRate : float32<rad/s/iu> =
        LanguagePrimitives.Float32WithMeasure(MathHelper.PiOver2)

    member this.MaxSideForce =
        15000.0f<N>

    member this.MaxVerticalForce =
        15000.0f<N>

    member this.MaxForwardThrust =
        150000.0f<N>

    member this.MaxBackwardThrust =
        30000.0f<N>

    member this.MaxForwardSpeed =
        240.0f<m/s>

    member this.MaxBackwardSpeed =
        50.0f<m/s>

    member this.TargetSpeedRate =
        30.0f<m/s^2/iu>


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

/// Extract device-independent orders from the gamepad of a given player index and personal settings.
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

    let pwr2(x : float32, y : float32) : float32 * float32 =
        let dist2 = x * x + y * y
        dist2 * x, dist2 * y

    let turnRight, turnUp = pwr2(turnRight, turnUp)

    { turnRight = 1.0f<iu> * turnRight
      turnUp = 1.0f<iu> * turnUp
      forwardSpeedAdjust = 1.0f<iu> * forwardSpeedAdjust
      fireRequested = fireRequested }
    
/// Extract controls from all local players' game pads.
let getAllControls settings playerIndices =
    List.map2 getControls settings playerIndices

/// Given inputs from local players, compute new heading, right vector, target speed and jet forces.
let handlePlayerInputs (dt : float32<s>) (players : Players) (ships : GameState.Ships) (controls : Controls list) =
    let clamp low hi x =
        x
        |> max low
        |> min hi

    let hrt =
        [
            for shipIdx, controls, targetSpeed in List.zip3 players.localPlayersIdxs controls players.localTargetSpeeds do
                let shipType = players.shipTypes.[shipIdx]
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
                
                yield (heading |> TypedVector.normalize3, right |> TypedVector.normalize3, targetSpeed)
        ]

    let forces =
        [
            for shipIdx, (heading, right, targetSpeed) in List.zip players.localPlayersIdxs hrt do
                let speed = ships.speeds.[shipIdx]
                let shipType = players.shipTypes.[shipIdx]
                let up = TypedVector.cross3(right, heading)
                let forceRight =
                    (computeSideForce shipType.SideDrag right speed
                     |> clamp -shipType.MaxSideForce shipType.MaxSideForce)
                    * right
                let forceUp =
                    (computeVerticalForce shipType.VerticalDrag heading right speed
                     |> clamp -shipType.MaxVerticalForce shipType.MaxVerticalForce)
                    * up
                let forceForward =
                    (computeThrust shipType.BreakDrag shipType.AccelDrag heading speed targetSpeed
                     |> clamp -shipType.MaxBackwardThrust shipType.MaxForwardThrust)
                    * heading
                yield forceRight + forceUp + forceForward
        ]

    let headings, rights, targetSpeeds = List.unzip3 hrt

    headings, rights, targetSpeeds, forces

let stop() =
    ()

/// Check which controls are requesting fire, validate those that are ready to fire, and return data needed to create new bullets.
/// This data is composed of:
/// - new ship states with updated special bullet counts,
/// - tuples to construct RemoteEvents, and
/// - the updated last bullet GUID.
let fireBullets bulletCount (players : GameState.Players) (ships : GameState.Ships) (controls : Controls list) =
    let rec work bulletCount localPlayers controls timeLeft numBigBullets numFastBullets numHiRateBullets numMultiFire =
        match localPlayers, controls, timeLeft, numBigBullets, numFastBullets, numHiRateBullets, numMultiFire with
        | player :: localPlayers, control :: controls, t :: timeLeft, bb :: numBigBullets, fb :: numFastBullets, hrb :: numHiRateBullets, mf :: numMultiFire ->
            let fireAuthorized =
                control.fireRequested && t <= 0<dms>

            let bulletCount =
                if fireAuthorized then
                    bulletCount + 1
                else
                    bulletCount
                
            let ret =
                if fireAuthorized then
                    stop()
                    let isBig = bb > 0
                    let isFast = fb > 0
                    let isHiRate = hrb > 0
                    let isMultiFire = mf > 0
                    
                    let guid = getGuid player bulletCount
                    let radius =
                        if isBig then BulletConstants.bigRadius else BulletConstants.radius
                    let speed =
                        if isFast then BulletConstants.fastSpeed else BulletConstants.speed
                    let heading = ships.headings.[player]
                    let dist0 = 5.0f<m>
                    let pos = ships.posClient.[player] + dist0 * heading

                    let newBullets =
                        if isMultiFire then
                            let up = TypedVector.cross3(ships.rights.[player], heading)
                            let rot = Quaternion.CreateFromAxisAngle(up, BulletConstants.multiFireSpread)
                            let shipSpeed = ships.speeds.[player]
                            [(guid, player, radius, pos, shipSpeed + speed * TypedVector3<1>(Vector3.Transform(heading.v, rot)))
                             (guid, player, radius, pos, shipSpeed + speed * TypedVector3<1>(Vector3.Transform(heading.v, -rot)))
                             (guid, player, radius, pos, shipSpeed + speed * heading)
                            ]
                        else
                            let speed = ships.speeds.[player] + speed * heading
                            [(guid, player, radius, pos, speed)]
                    (
                        ((if isHiRate then dmsFromS BulletConstants.highRateFirePeriod else dmsFromS BulletConstants.firePeriod),
                         (if isBig then bb - 1 else bb),
                         (if isFast then fb - 1 else fb),
                         (if isHiRate then hrb - 1 else hrb),
                         (if isMultiFire then mf - 1 else mf)),
                        newBullets
                    )
                else
                    (
                        (t, bb, fb, hrb, mf),
                        []
                    )

            ret :: work bulletCount localPlayers controls timeLeft numBigBullets numFastBullets numHiRateBullets numMultiFire
        | [], [], [], [], [], [], [] -> []
        | _ -> failwith "List lengths don't match"

    let tmp = 
        work bulletCount players.localPlayersIdxs controls players.timeBeforeFire players.numBigBullets players.numFastBullets players.numHighRate players.numMultiFire

    let timeLeft =
        tmp
        |> List.map (fun ((x, _, _, _, _), _) -> x)

    let numBigBullets =
        tmp
        |> List.map (fun ((_, x, _, _, _), _) -> x)

    let numFastBullets =
        tmp
        |> List.map (fun ((_, _, x, _, _), _) -> x)

    let numHighRate =
        tmp
        |> List.map (fun ((_, _, _, x, _), _) -> x)

    let numMultiFire =
        tmp
        |> List.map (fun ((_, _, _, _, x), _) -> x)

    let newBulletsData =
        tmp
        |> List.map snd
        |> List.concat

    let bulletCount =
        if List.isEmpty newBulletsData then
            bulletCount
        else
            let bulletCount =
                newBulletsData
                |> List.map (fun (x, _, _, _, _) -> countOfBulletGuid x)
                |> List.max
            bulletCount

    { players with
        timeBeforeFire = timeLeft
        numBigBullets = numBigBullets
        numFastBullets = numFastBullets
        numHighRate = numHighRate
        numMultiFire = numMultiFire
    }
    ,
    newBulletsData
    ,
    bulletCount
