module Asteroids.TrajectoryCollision

open Microsoft.Xna.Framework
open System

let NO_INTERSECTION = Single.NaN
let isNoIntersection x = Single.IsNaN x

let (|NoIntersection|_|) x = if Single.IsNaN x then Some() else None
let (|IntersectionAt|_|) x = if Single.IsNaN x then None else Some x

let getIntersectionTime (pos1 : Vector3, vel1 : Vector3, r1 : float32,
                         pos2 : Vector3, vel2 : Vector3, r2 : float32) =
    let sq x = x*x

    let U = vel1 - vel2
    let a = U.LengthSquared()
    if a >= 1.0e-6f
    then
        let P = pos1 - pos2
        let R = r1 + r2
        let R2 = sq R
        let b = 2.0f * Vector3.Dot(P, U)
        let c = P.LengthSquared() - R2
        let Δ = sq b - 4.0f * a * c
        
        if Δ > 0.0f
        then
            let Δ' = Math.Sqrt(float Δ) |> float32
            let r1 = (-b - Δ')/(2.0f * a)
            let r2 = (-b + Δ')/(2.0f * a)
            
            if r1 < 0.0f
            then
                if r2 >= 0.0f
                then
                    0.0f
                else
                    NO_INTERSECTION
            else
                r1
        
        elif Δ = 0.0f then
            let r = -b / (2.0f * a)
            if r < 0.0f
            then
                NO_INTERSECTION
            else
                r
        
        else
            let r = -b / (2.0f * a)
            if r >= 0.0f
            then
                let pos1 = pos1 + r * vel1
                let pos2 = pos2 + r * vel2
                NO_INTERSECTION
            else
                NO_INTERSECTION
    else
        let P = pos1 - pos2
        let R = r1 + r2
        let R2 = sq R
        if P.LengthSquared() <= R2
        then
            0.0f
        else
            NO_INTERSECTION
