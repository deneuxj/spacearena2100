module SpaceArena2100.TargetingHud

open Microsoft.Xna.Framework
open Microsoft.Xna.Framework.Graphics

open CleverRake.XnaUtils
open CleverRake.XnaUtils.Units
open CleverRake.XnaUtils.ScreenCoordinates

/// Indicates the direction of the relative speed of a ship relatively to another ship.
type DirectionMarkers =
    { angle : float32<rad> option // 0 -> other flying to the right, PI/2 -> other flying up, None -> flying like me.
      away : float32 // -1 -> other flying towards me, +1 -> other flying away from me.
    }


let computeOtherDirectionMarkers (heading : TypedVector3<1>) (right : TypedVector3<1>) (speed : TypedVector3<m/s>) (otherSpeed : TypedVector3<m/s>) =
    let relativeSpeed = otherSpeed - speed
    let relativeDirection = TypedVector.tryNormalize3 relativeSpeed

    match relativeDirection with
    | None -> { angle = None; away = 0.0f }
    | Some S ->
        let up = TypedVector.cross3(right, heading)
        let lx = TypedVector.dot3(relativeSpeed, right)
        let ly = TypedVector.dot3(relativeSpeed, up)
        let x = TypedVector.dot3(S, right)
        let y = TypedVector.dot3(S, up)
        let angle =
            if sqrt(lx * lx + ly * ly) < 10.0f<m/s> then
                None
            else
                Some (atan2 x y * 1.0f<rad>)
        let away = TypedVector.dot3(S, heading)
        { angle = angle ; away = away }


let getPosOnScreen (fov : float32<rad>) (ratio : float32) (heading : TypedVector3<1>) (right : TypedVector3<1>) (pos : TypedVector3<m>) (other : TypedVector3<m>) =
    let up = TypedVector.cross3(right, heading)

    let d0 = 0.5f<m> // Distance to screen (need not be accurate)
    let h0 = d0 * tan (0.5f * float32 fov)
    let w0 = h0 * ratio

    let diff = other - pos
    let d = TypedVector.dot3(diff, heading)
    if d > 5.0f<m> then
        let w = TypedVector.dot3(diff, right)
        let h = TypedVector.dot3(diff, up)
    
        let k = d0 / d
        let wp = k * w
        let hp = k * h
    
        Some (1.0f<Std> * wp / w0, 1.0f<Std> * hp / h0)
    else
        None


open SpaceArena2100.Rendering

let renderTargeting (sb : SpriteBatch) markerTexture circleTexture ww hh fov ratio heading right pos speed otherPos otherSpeed =
    let posOnScreen =
        otherPos
        |> Array.map (getPosOnScreen fov ratio heading right pos)

    let markers =
        otherSpeed
        |> Array.map (computeOtherDirectionMarkers heading right speed)

    try
        let markerH = 0.1f<Std>
        let markerW = markerH / ratio

        sb.Begin()
        for (pos, marker) in Seq.zip posOnScreen markers do
            match pos, marker.angle with
            | Some(x, y), Some angle ->
                sb.Draw(
                    markerTexture,
                    Rectangle.NewFromStd(ww, hh, markerW, markerH, x, y),
                    angle,
                    Color.Yellow)
            | Some(x, y), None ->
                sb.Draw(
                    circleTexture,
                    Rectangle.NewFromStd(ww, hh, markerW, markerH, x, y),
                    0.0f<rad>,
                    Color.Yellow)
            | None, _ -> ()
    finally
        sb.End()
