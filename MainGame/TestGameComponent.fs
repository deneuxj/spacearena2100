module SpaceArena2100.TestGameComponent

open Microsoft.Xna.Framework

open CleverRake.XnaUtils
open CleverRake.XnaUtils.Units

open InstancedModel

open SpaceArena2100.GameState
open SpaceArena2100.GameStateUpdate


let newDescription() : Description =
    let pos = MarkedArray [| TypedVector3<m>(0.0f<m>, 0.0f<m>, 0.0f<m>) |] 
    let radius = MarkedArray [| 10.0f<m> |]

    let newBoundingSphere (center : TypedVector3<m>) (radius : float32<m>) =
        new BoundingSphere(center.v, float32 radius)

    let boundingSpheres =
        Array.map2 newBoundingSphere pos.Content radius.Content
        |> MarkedArray

    let boundingSpace =
        new BoundingBox()

    let _, octree =
        boundingSpheres.Content
        |> Array.fold (fun (idx : int<AstIdx>, octree : Octree.Node<int<AstIdx> list>) sphere ->
                idx + 1<AstIdx>
                ,
                Octree.insert 10 10 (fun (bbox : BoundingBox) idx2 -> boundingSpheres.[idx2].Intersects(bbox)) octree idx 0
            )
            (0<AstIdx>, Octree.newEmpty boundingSpace)

    let asteroids : Asteroids =
        { pos = pos;
          radius = radius;
          rotations = MarkedArray [| Quaternion.Identity |];
          octree = octree;
          fieldSizes = TypedVector3<m>(1000.0f<m>, 1000.0f<m>,1000.0f<m>);
        }

    { numPlayers = 1;
      myHostId = 1;
      playerNames = MarkedArray [| "Tester" |];
      localPlayersIdxs = [ 0<GPI> ];
      localAiPlayerIdxs = [];
      remotePlayerIdxs = [];
      shipTypes = MarkedArray [| Bull |];
      gonePlayerIdxs = [];
      asteroids = asteroids;
    }


let newComponent (game : Game) =
    let initialState = failwith "TODO"

    let content = new Content.ContentManager(game.Services)
    let gdm = new GraphicsDeviceManager(game)

    let shipModel = ref None
    let asteroidModel = ref None
    let shipRenderer = ref None
    let asteroidRenderer = ref None

    let initialize () =
        let ship = content.Load<Graphics.Model>("ship")
        let asteroid = content.Load<Graphics.Model>("asteroid")
        shipRenderer := Some <| new InstancedModelRenderer(gdm, ship)
        asteroidRenderer := Some <| new InstancedModelRenderer(gdm, asteroid)
        shipModel := Some ship
        asteroidModel := Some asteroid

    let dispose() =
        content.Dispose()
        (gdm :> System.IDisposable).Dispose()

    let update (gt : GameTime) state = state
    let compute (gt : GameTime) state = state
    let draw (gt : GameTime) state = ()

    let comp = new ParallelUpdateDrawGameComponent<State>(game, initialState, initialize, update, compute, draw, dispose)
    comp