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
        { headings = MarkedArray [| TypedVector3<1>(0.0f, 0.0f, 1.0f) |]
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

open CleverRake.XnaUtils.EvilNull

let newComponent (game : Game) =
    let description = newDescription()
    let initialState = newInitialState(), System.TimeSpan.FromSeconds(0.0)

    let content = new Content.ContentManager(game.Services)
    let gdm =
        match game.Services.GetService(typeof<IGraphicsDeviceManager>) with
        | NonNull (:? GraphicsDeviceManager as x) -> x
        | NonNull x -> failwithf "Expected a GraphicsDeviceManager, got %s" (x.GetType().Name)
        | Null -> failwith "No IGraphicsDeviceManager found"

    let shipModel = ref None
    let asteroidModel = ref None
    let shipRenderer = ref None
    let asteroidRenderer = ref None
    let spriteBatch = ref None
    let font = ref None

    let initialize() =
        let ship = content.Load<Graphics.Model>("Content\\ships\\finch")
        let asteroid = content.Load<Graphics.Model>("Content\\asteroids\\asteroid01")
        let sb = new Graphics.SpriteBatch(gdm.GraphicsDevice)
        let courier = content.Load<Graphics.SpriteFont>("Content\\courier")

        let instancingTechnique =
            match gdm.GraphicsProfile with
            | Graphics.GraphicsProfile.HiDef -> InstancingTechnique.HardwareInstancing
            | Graphics.GraphicsProfile.Reach -> InstancingTechnique.NoInstancing
            | _ -> failwith "Unknown graphics profile"
        shipRenderer := Some <| new InstancedModelRenderer(gdm, ship, InstancingTechnique = instancingTechnique)
        asteroidRenderer := Some <| new InstancedModelRenderer(gdm, asteroid, InstancingTechnique = instancingTechnique)
        
        shipModel := Some ship
        asteroidModel := Some asteroid
        spriteBatch := Some sb
        font := Some courier

    let dispose() =
        content.Dispose()
        match spriteBatch.Value with
        | Some sb -> sb.Dispose()
        | None -> ()
        (gdm :> System.IDisposable).Dispose()

    let settings =
        description.localPlayersIdxs
        |> List.map (fun _ -> ShipControl.defaultSettings)

    let update (gt : GameTime) (state : State, computationTime) =
        let me = 0<GPI>
        let dt = 1.0f<s> * (gt.ElapsedGameTime.TotalSeconds |> float32)

        let controls =
            ShipControl.handlePlayerInputs dt description.localPlayersIdxs settings [ PlayerIndex.One ] state.ships description.shipTypes
        
        (state.ships.posHost.[me],
         state.ships.headings.[me],
         state.ships.rights.[me],
         computationTime
        ),
        (state, controls)

    let watch = new System.Diagnostics.Stopwatch()

    let compute (gt : GameTime) (state, (headings, rights, targetSpeeds, forces)) =
        watch.Restart()

        let dt = 1.0f<s> * (gt.ElapsedGameTime.TotalSeconds |> float32)
        let state' = GameStateUpdate.update dt description [||] forces headings rights state

        state', watch.Elapsed

    let renderAsteroids = Rendering.renderAsteroids description.asteroids.pos.Content description.asteroids.rotations.Content description.asteroids.radius.Content description.asteroids.fieldSizes
    
    let draw (gt : GameTime) (pos, heading, right, computationTime : System.TimeSpan) =
        match asteroidRenderer.Value, spriteBatch.Value, font.Value with
        | Some r, Some sb, Some font ->
            renderAsteroids r pos heading right
            try
                sb.Begin()
                sb.DrawString(font, sprintf "%3.1f %%" (100.0 * computationTime.TotalSeconds / gt.ElapsedGameTime.TotalSeconds), Vector2(100.0f, 100.0f), Color.White)
                sb.DrawString(font, sprintf "%A" pos.v, Vector2(100.0f, 130.0f), Color.White)
                sb.DrawString(font, sprintf "%A" heading.v, Vector2(100.0f, 160.0f), Color.White)
                sb.DrawString(font, sprintf "%A" right.v, Vector2(100.0f, 190.0f), Color.White)
            finally
                sb.End()
        | _ -> ()

    let comp = new ParallelUpdateDrawGameComponent<_, _, _>(game, initialState, initialize, update, compute, draw, dispose)
    comp