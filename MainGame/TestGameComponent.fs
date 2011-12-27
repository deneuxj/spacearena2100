module SpaceArena2100.TestGameComponent

open Microsoft.Xna.Framework

open CleverRake.XnaUtils
open CleverRake.XnaUtils.Units

open InstancedModel

open SpaceArena2100.GameState
open SpaceArena2100.GameStateUpdate
open SpaceArena2100.Units

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

let newInitialState() : State =
    let pos = MarkedArray [| TypedVector3<m>(0.0f<m>, 0.0f<m>, -50.0f<m>) |]
    let ships : Ships =
        { headings = MarkedArray [| TypedVector3<1>(0.0f, 0.0f, -1.0f) |]
          rights = MarkedArray [| TypedVector3<1>(1.0f, 0.0f, 0.0f) |]
          posClient = pos
          posVisible = pos
          posHost = pos
          posLerpT = MarkedArray [| 0.0f |]
          speeds = MarkedArray [| TypedVector3<m/s>() |]
          accels = MarkedArray [| TypedVector3<m/s^2>() |]
          health = MarkedArray [| 1.0f<Health> |]
          numFastBullets = [ 0 ]
          numBigBullets = [ 0 ]
          numMultiFire = [ 0 ]
          numHighRate = [ 0 ]
          timeBeforeFire = [ 0<dms> ]
          timeBeforeRespawn = [ -1<dms> ]
          localTargetSpeeds = [ 0.0f<m/s> ]
          scores = MarkedArray [| 0.0f<Points> |]
        }

    let bullets : Bullets =
        { guids = [||]
          pos = [||]
          timeLeft = [||]
          speeds = [||]
          radii = [||]
          owners = [||]
        }

    let supplies : Supplies =
        { pos = MarkedArray [||]
          types = MarkedArray [||]
          radii = MarkedArray [||]
        }

    { ships = ships ; bullets = bullets ; supplies = supplies ; time = 0<dms> }

let newComponent (game : Game) =
    let description = newDescription()
    let initialState = newInitialState()

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

    let settings =
        description.localPlayersIdxs
        |> List.map (fun _ -> ShipControl.defaultSettings)

    let update (gt : GameTime) state = state
    let compute (gt : GameTime) state = state
    let draw (gt : GameTime) state = ()

    let comp = new ParallelUpdateDrawGameComponent<State>(game, initialState, initialize, update, compute, draw, dispose)
    comp