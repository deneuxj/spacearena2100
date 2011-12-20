module SpaceArena2100.WarpCoord

open Microsoft.Xna.Framework

let floor x =
    x |> double |> System.Math.Floor |> float32

/// x -> [-wx/2, +wx/2]
let warp1 wx (x : float32) =
    let wx = wx / 2.0f
    let x = 0.5f * (wx + x)
    let div = floor (x / wx) 
    let rem = x - (div * wx)
    rem / 0.5f - wx


let warp (w : Vector3) (pos : Vector3) =
    let x = warp1 w.X pos.X
    let y = warp1 w.Y pos.Y
    let z = warp1 w.Z pos.Z
    Vector3(x, y, z)


let diff w x y =
    x - y
    |> warp w
    
    
let distSq w x y =
    x - y
    |> warp w
    |> fun x -> x.LengthSquared()


let dist w x y =
    x - y
    |> warp w
    |> fun x -> x.Length()
    

let getAlternativePositions (V : Vector3) (pos : Vector3) (dir : Vector3) =
    let difX =
        2.0f *
        (if dir.X > 0.0f
            then
                -Vector3.UnitX * V.X
            else
                Vector3.UnitX * V.X)
    let difY =
        2.0f *
        (if dir.Y > 0.0f
            then
                -Vector3.UnitY * V.Y
            else
                Vector3.UnitY * V.Y)
    let difZ =
        2.0f *
        (if dir.Z > 0.0f
            then
                -Vector3.UnitZ * V.Z
            else
                Vector3.UnitZ * V.Z)

    [|    
        for i in 0..7 ->
            (if (i &&& 1) <> 0 then difX else Vector3.Zero)
            + (if (i &&& 2) <> 0 then difY else Vector3.Zero)
            + (if (i &&& 4) <> 0 then difZ else Vector3.Zero)
    |]