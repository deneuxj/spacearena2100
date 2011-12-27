module SpaceArena2100.Rendering

open Microsoft.Xna.Framework

open CleverRake.XnaUtils
open CleverRake.XnaUtils.Units

open InstancedModel

let fieldOfView = MathHelper.ToRadians(60.0f)
let ratio = 16.0f / 9.0f
let nearPlane = 1.0f
let farPlane = 1e4f

let renderAsteroids (renderer : InstancedModelRenderer) (fieldSize : float32<m>) (position : TypedVector3<m>) (heading : TypedVector3<1>) (right : TypedVector3<1>) (positions : TypedVector3<m>[]) (rotations : Quaternion[]) (radii : float32<m>[]) =
    let V = Vector3(float32 fieldSize, float32 fieldSize, float32 fieldSize)

    let view =
        Matrix.CreateLookAt(Vector3.Zero, heading.v, TypedVector.cross3(right, heading).v)

    let projection =
        Matrix.CreatePerspectiveFieldOfView(fieldOfView, ratio, nearPlane, farPlane)

    let positions =
        positions
        |> Array.map (fun pos -> (pos - position).v |> WarpCoord.warp V)

    let computeTransform (pos : Vector3) (rotation : Quaternion) =
        Matrix.CreateFromQuaternion(rotation)
        *
        Matrix.CreateTranslation(pos)

    let transforms =
        Array.map2 computeTransform positions rotations

    renderer.Draw(transforms, view, projection)

let renderShips (renderer : InstancedModelRenderer) (position : TypedVector3<m>) (heading : TypedVector3<1>) (right : TypedVector3<1>) (positions : TypedVector3<m>[]) (headings : TypedVector3<1>[]) (rights : TypedVector3<1>[]) =
    let view =
        Matrix.CreateLookAt(position.v, position.v + heading.v, TypedVector.cross3(right, heading).v)

    let projection =
        Matrix.CreatePerspectiveFieldOfView(fieldOfView, ratio, nearPlane, farPlane)

    let inline computeTransform (pos : TypedVector3<m>) (heading : TypedVector3<1>) (right : TypedVector3<1>) =
        Matrix.CreateWorld(pos.v, heading.v, TypedVector.cross3(right, heading).v)

    let transforms =
        ArrayInlined.map3 computeTransform positions headings rights

    renderer.Draw(transforms, view, projection)
