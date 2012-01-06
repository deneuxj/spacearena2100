module SpaceArena2100.Rendering

open Microsoft.Xna.Framework

open CleverRake.XnaUtils
open CleverRake.XnaUtils.Units

open InstancedModel

let fieldOfView = MathHelper.ToRadians(60.0f)
let ratio = 16.0f / 9.0f
let nearPlane = 1.0f
let farPlane = 1e4f


module Quads =
    open Microsoft.Xna.Framework.Graphics

    let newQuad () =
        let corners =
            [|(-1, -1) ; (1, -1) ; (1, 1) ; (-1, 1)|]
            |> Array.map (fun (x, y) -> 0.5f * new Vector3(float32 x, float32 y, 0.0f))
    
        let textCoords =
            [|(0, 1) ; (1, 1) ; (1, 0); (0, 0)|]
            |> Array.map (fun (x, y) -> new Vector2(float32 x, float32 y))
    
        let vertices =
            Array.zip corners textCoords
            |> Array.map (fun (v, t) -> new VertexPositionNormalTexture(v, Vector3.UnitZ, t))
    
        let indexes : int16[]=
            [| 0s; 1s; 2s; 2s; 3s; 0s |]
    
        (vertices, indexes, VertexPositionNormalTexture.VertexDeclaration)

    let getVertices (verts, _, _) : VertexPositionNormalTexture[] = verts
    let getIndexes (_, indexes, _) : int16 [] = indexes
    let getDeclaration (_, _, d) : VertexDeclaration = d

    let theQuad = newQuad()

    /// Render a number of quads facing the camera.
    let renderBillboards setWorld (dev : GraphicsDevice) (center : TypedVector3<m>) (forward : TypedVector3<1>) (up : TypedVector3<1>) (size : float32<m>) (positions : TypedVector3<m>[]) (effect : Effect) =
        let S = Matrix.CreateScale(float32 size)

        for pos in positions do
            let T = Matrix.CreateBillboard(pos.v, center.v, up.v, System.Nullable(forward.v))

            setWorld(S * T)

            for pass in effect.CurrentTechnique.Passes do
                pass.Apply()
                dev.DrawUserIndexedPrimitives<VertexPositionNormalTexture>(PrimitiveType.TriangleList, getVertices theQuad, 0, 4, getIndexes theQuad, 0, 2)


let renderAsteroids (scaling : float32) (positions : TypedVector3<m>[]) (rotations : Quaternion[]) (radii : float32<m>[]) (V : TypedVector3<m>) (renderer : InstancedModelRenderer) (position : TypedVector3<m>) (heading : TypedVector3<1>) (right : TypedVector3<1>) =
    let view =
        Matrix.CreateLookAt(Vector3.Zero, heading.v, TypedVector.cross3(right, heading).v)

    let projection =
        Matrix.CreatePerspectiveFieldOfView(fieldOfView, ratio, nearPlane, farPlane)

    let positions =
        positions
        |> Array.map (fun pos -> (pos - position).v |> WarpCoord.warp V.v)

    let computeTransform (pos : Vector3) (rotation : Quaternion) (radius : float32<m>) =
        Matrix.CreateFromQuaternion(rotation)
        *
        Matrix.CreateScale((float32 radius) * scaling)
        *
        Matrix.CreateTranslation(pos)

    let transforms =
        ArrayInlined.map3 computeTransform positions rotations radii
        |> ArrayInlined.filterRef (fun (pos : Vector3) -> Vector3.Dot(pos, heading.v) >= 0.0f) positions

    renderer.Draw(transforms, view, projection)


let renderBullets dev (effect : Graphics.Effect) setView setProjection setWorld (positions : TypedVector3<m>[]) (radii : float32<m>[]) (position : TypedVector3<m>) (heading : TypedVector3<1>) (right : TypedVector3<1>) =
    let up = TypedVector.cross3(right, heading)

    let view =
        Matrix.CreateLookAt(position.v, position.v + heading.v, up.v)

    let projection =
        Matrix.CreatePerspectiveFieldOfView(fieldOfView, ratio, nearPlane, farPlane)
    
    setView view
    setProjection projection

    Quads.renderBillboards setWorld dev position heading up 1.0f<m> positions effect


let renderShips (renderer : InstancedModelRenderer) (position : TypedVector3<m>) (heading : TypedVector3<1>) (right : TypedVector3<1>) (positions : TypedVector3<m>[]) (headings : TypedVector3<1>[]) (rights : TypedVector3<1>[]) (shipTypes : GameState.ShipType[]) =
    let up = TypedVector.cross3(right, heading).v
    let view =
        Matrix.CreateLookAt(position.v, position.v + heading.v, up)

    let projection =
        Matrix.CreatePerspectiveFieldOfView(fieldOfView, ratio, nearPlane, farPlane)

    let inline computeTransform (pos : TypedVector3<m>) (heading : TypedVector3<1>) (right : TypedVector3<1>) (shipType : GameState.ShipType) =
        Matrix.CreateScale(float32 shipType.BoundingSphereRadius)
        *
        Matrix.CreateWorld(pos.v, heading.v, up)

    let transforms =
        ArrayInlined.map4 computeTransform positions headings rights shipTypes
        |> ArrayInlined.filterRef (fun pos -> pos <> position) positions // Don't render my ship.
    
    renderer.Draw(transforms, view, projection)
