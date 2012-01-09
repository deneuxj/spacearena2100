module SpaceArena2100.TestGameComponent

open Microsoft.Xna.Framework

open CleverRake.XnaUtils
open CleverRake.XnaUtils.Units

open InstancedModel

open SpaceArena2100.GameState
open SpaceArena2100.GameStateUpdate
open SpaceArena2100.Units

let newDescription() : Description =
    let random = new System.Random()
    let fieldSize = 5000.0f<m>
    let randomPos() =
        (double fieldSize
         *
         0.5
         *
         random.NextDouble()
         *
         if random.Next(0, 2) = 0 then 1.0 else -1.0)
        |> float32
    
    let randomVec3() =
        new Vector3(randomPos(), randomPos(), randomPos())

    let randomRotation() =
        let vec = randomVec3()
        vec.Normalize()
        Quaternion.CreateFromAxisAngle(vec, float32(random.NextDouble()) * MathHelper.Pi)

    let numAsteroids = 2000
    
    let pos =
        Array.init numAsteroids (fun _ -> TypedVector3<m>(randomVec3()))
        |> MarkedArray

    let radius =
        Array.init numAsteroids (fun _ -> 1.0f<m> * float32 (10.0 * random.NextDouble() + 5.0))
        |> MarkedArray

    let newBoundingSphere (center : TypedVector3<m>) (radius : float32<m>) =
        new BoundingSphere(center.v, float32 radius)

    let boundingSpheres =
        Array.map2 newBoundingSphere pos.Content radius.Content
        |> MarkedArray

    let rotations =
        Array.init numAsteroids (fun _ -> randomRotation())
        |> MarkedArray

    let boundingSpace =
        new BoundingBox(float32 fieldSize / 2.0f * -Vector3.One, float32 fieldSize / 2.0f * Vector3.One)

    let _, octree =
        boundingSpheres.Content
        |> Array.fold (fun (idx : int<AstIdx>, octree : Octree.Node<int<AstIdx> list>) sphere ->
                idx + 1<AstIdx>
                ,
                Octree.insert 5 10 (fun (bbox : BoundingBox) idx2 -> boundingSpheres.[idx2].Intersects(bbox)) octree idx 0
            )
            (0<AstIdx>, Octree.newEmpty boundingSpace)

    let asteroids : Asteroids =
        { pos = pos;
          radius = radius;
          rotations = rotations;
          octree = Octree.freeze octree;
          fieldSizes = fieldSize * TypedVector3<1>(Vector3.One);
        }

    { numPlayers = 1;
      myHostId = 1;
      playerNames = MarkedArray [| "Tester" |];
      localPlayersIdxs = [ 0<GPI> ; 1<GPI> ];
      localAiPlayerIdxs = [ 1<GPI> ];
      remotePlayerIdxs = [];
      shipTypes = MarkedArray [| Bull ; Bull |];
      gonePlayerIdxs = [];
      asteroids = asteroids;
    }

let newInitialState (localAiPlayerIdxs : int<GPI> list) : State =
    let pos = MarkedArray [| TypedVector3<m>(0.0f<m>, 0.0f<m>, -50.0f<m>) ; TypedVector3<m>() |]
    let ships : Ships =
        { headings = MarkedArray [| TypedVector3<1>(0.0f, 0.0f, 1.0f) ; TypedVector3<1>(Vector3.UnitX) |]
          rights = MarkedArray [| TypedVector3<1>(1.0f, 0.0f, 0.0f) ; TypedVector3<1>(Vector3.UnitY) |]
          posClient = pos
          posVisible = pos
          posHost = pos
          posLerpT = MarkedArray [| 0.0f ; 0.0f |]
          speeds = MarkedArray [| TypedVector3<m/s>() ; TypedVector3<m/s>() |]
          accels = MarkedArray [| TypedVector3<m/s^2>() ; TypedVector3<m/s^2>() |]
          health = MarkedArray [| 1.0f<Health> ; 1.0f<Health> |]
          numFastBullets = [ 0 ; 0 ]
          numBigBullets = [ 0 ; 0 ]
          numMultiFire = [ 0 ; 0 ]
          numHighRate = [ 0 ; 0 ]
          timeBeforeFire = [ 0<dms> ; 0<dms> ]
          timeBeforeRespawn = [ -1<dms> ; -1<dms> ]
          localTargetSpeeds = [ 0.0f<m/s> ; 0.0f<m/s> ]
          scores = MarkedArray [| 0.0f<Points> ; 0.0f<Points> |]
        }

    let bullets : Bullets =
        { guids = [||]
          pos = [||]
          timeLeft = [||]
          speeds = [||]
          radii = [||]
          owners = [||]
          lastLocalGuid = firstGuid 1
        }

    let supplies : Supplies =
        { pos = MarkedArray [||]
          types = MarkedArray [||]
          radii = MarkedArray [||]
        }

    let ais =
        localAiPlayerIdxs
        |> List.map (fun _ -> Undecided)

    { ships = ships ; ais = ais ; bullets = bullets ; supplies = supplies ; time = 0<dms> }

open CleverRake.XnaUtils.EvilNull

let newComponent (game : Game) =
    let description = newDescription()
    let initialState = newInitialState description.localAiPlayerIdxs, System.TimeSpan.FromSeconds(0.0)

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
    let effect = ref None

    let initialize() =
        let ship = content.Load<Graphics.Model>("Content\\ships\\finch")
        let asteroid = content.Load<Graphics.Model>("Content\\asteroids\\asteroid02")
        let sb = new Graphics.SpriteBatch(gdm.GraphicsDevice)
        let courier = content.Load<Graphics.SpriteFont>("Content\\courier")
        let fx =
            let texture = content.Load<Graphics.Texture2D>("Content\\fireball")
            new Graphics.BasicEffect(gdm.GraphicsDevice, TextureEnabled = true, Texture = texture)

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
        effect := Some fx

    let dispose() =
        content.Unload()
        match spriteBatch.Value with
        | Some sb -> sb.Dispose()
        | None -> ()
        match effect.Value with
        | Some fx -> fx.Dispose()
        | None -> ()
        (gdm :> System.IDisposable).Dispose()

    let settings =
        description.localPlayersIdxs
        |> List.map (fun _ -> ShipControl.defaultSettings)

    let update (gt : GameTime) (state : State, computationTime) =
        let me = 0<GPI>
        let ai = 1<GPI>

        let dt = 1.0f<s> * (gt.ElapsedGameTime.TotalSeconds |> float32)

        let humanControls =
            ShipControl.getAllControls settings [ Some PlayerIndex.One ; None ]

        let aiStates, aiControls =
            List.zip description.localAiPlayerIdxs state.ais
            |> List.map (fun (idxAi, ai) -> AiSteering.updateAi (getBulletSpeed description.localAiPlayerIdxs state.ships) dt state description ai idxAi)
            |> List.unzip

        let rec work humanControls aiControls humanPlayers aiPlayers =
            match humanControls, aiControls, humanPlayers, aiPlayers with
            | [], _, _, _ -> aiControls
            | _, [], _, _ -> humanControls
            | hc :: restHC, aic :: restAIC, hidx :: restHIDX, aiidx :: restAIIDX ->
                if hidx = aiidx then
                    aic :: work restHC restAIC restHIDX restAIIDX
                else
                    hc :: work restHC aiControls restHIDX aiPlayers
            | _ -> failwith "Missing data"

        let controls = //humanControls
            work humanControls aiControls description.localPlayersIdxs description.localAiPlayerIdxs

        let ships, newBullets, lastLocalGuid =
            ShipControl.fireBullets state.bullets.lastLocalGuid description.localPlayersIdxs state.ships controls

        let controls =
            ShipControl.handlePlayerInputs dt description.localPlayersIdxs ships description.shipTypes controls

        let state =
            { state with
                ships = ships
                ais = aiStates
                bullets = { state.bullets with lastLocalGuid = lastLocalGuid } }

        let timedEvents =
            newBullets
            |> List.map (fun data -> { time = state.time; event = RemoteEvent.BulletFired data } )
            |> Array.ofList

        let subject = ai
        (ships.posHost.[subject],
         ships.headings.[subject],
         ships.rights.[subject],
         ships.speeds.[subject],
         computationTime,
         state.bullets.pos,
         state.bullets.radii,
         state.ships.posVisible.Content,
         state.ships.headings.Content,
         state.ships.rights.Content,
         description.shipTypes.Content
        )
        ,
        (state, controls, timedEvents)

    let watch = new System.Diagnostics.Stopwatch()

    let compute (gt : GameTime) (state : GameState.State, (headings, rights, targetSpeeds, forces), timedEvents) =
        watch.Restart()
        let state =
            { state with
                ships = { state.ships with localTargetSpeeds = targetSpeeds } }
        let dt = 1.0f<s> * (gt.ElapsedGameTime.TotalSeconds |> float32)
        let state' = GameStateUpdate.update dt description timedEvents forces headings rights state

        state', watch.Elapsed

    let renderAsteroids = Rendering.renderAsteroids (1.0f / 200.0f) description.asteroids.pos.Content description.asteroids.rotations.Content description.asteroids.radius.Content description.asteroids.fieldSizes
    
    let draw (gt : GameTime) (pos, heading, right, speed : TypedVector3<m/s>, computationTime : System.TimeSpan, bulletPos, bulletRadii, shipPos, shipHeadings, shipRights, shipTypes) =
        match asteroidRenderer.Value, shipRenderer.Value, spriteBatch.Value, font.Value, effect.Value with
        | Some r, Some r2, Some sb, Some font, Some effect ->
            renderAsteroids r pos heading right
            
            Rendering.renderBullets
                gdm.GraphicsDevice
                effect
                (fun V -> effect.View <- V)
                (fun P -> effect.Projection <- P)
                (fun W -> effect.World <- W)
                bulletPos
                bulletRadii
                pos
                heading
                right

            Rendering.renderShips
                r2
                pos
                heading
                right
                shipPos
                shipHeadings
                shipRights
                shipTypes
            try
                sb.Begin()
                sb.DrawString(font, sprintf "%3.1f %%" (100.0 * computationTime.TotalSeconds / 0.016667), Vector2(100.0f, 100.0f), Color.White)
                sb.DrawString(font, sprintf "%4.2f" (TypedVector.dot3(speed, heading) |> float32), Vector2(130.0f, 220.0f), Color.White)
            finally
                sb.End()
        | _ -> ()

    let comp = new ParallelUpdateDrawGameComponent<_, _, _>(game, initialState, initialize, update, compute, draw, dispose)
    comp