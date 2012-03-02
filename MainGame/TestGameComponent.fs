module SpaceArena2100.TestGameComponent

open Microsoft.Xna.Framework
open Microsoft.Xna.Framework.Net

open CleverRake.XnaUtils
open CleverRake.XnaUtils.Units
open CleverRake.XnaUtils.Dispose
open CleverRake.XnaUtils.ScreenCoordinates

open InstancedModel

open SpaceArena2100.GameState
open SpaceArena2100.GameStateUpdate
open SpaceArena2100.Units


open CleverRake.XnaUtils.EvilNull
open CleverRake.XnaUtils.CoopMultiTasking
open CleverRake.XnaUtils.XnaExtensions

type RenderResources =
    { shipModel : Graphics.Model
      asteroidModel : Graphics.Model
      spriteBatch : Graphics.SpriteBatch
      font : Graphics.SpriteFont
      effect : Graphics.BasicEffect
      shipRenderer : InstancedModelRenderer
      asteroidRenderer : InstancedModelRenderer
      radar : Graphics.RenderTarget2D
      radarTexture : Graphics.Texture2D
      dot : Graphics.Texture2D
      marker : Graphics.Texture2D
      circle : Graphics.Texture2D
      log : TextIconLogging.Log
    }


let newComponent (game : Game, description : Description, initialState, session, seed, size, random) =

    let participants = new Network.Participants(session, seed, size, random)
        
    let content = new Content.ContentManager(game.Services)
    let gdm =
        match game.Services.GetService(typeof<IGraphicsDeviceManager>) with
        | NonNull (:? GraphicsDeviceManager as x) -> x
        | NonNull x -> failwithf "Expected a GraphicsDeviceManager, got %s" (x.GetType().Name)
        | Null -> failwith "No IGraphicsDeviceManager found"

    let renderResources = ref None

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

        let radar = new Graphics.RenderTarget2D(gdm.GraphicsDevice, Rendering.ShipRadar.width, Rendering.ShipRadar.height)

        let radarTexture = content.Load<Graphics.Texture2D>("Content\\radar")
        let dotTexture = content.Load<Graphics.Texture2D>("Content\\spot")

        let marker = content.Load<Graphics.Texture2D>("Content\\marker")
        let circle = content.Load<Graphics.Texture2D>("Content\\circle")

        let log = TextIconLogging.Log([||], [||], courier, 0, 0, 20.0f, 5, 5.0f<s>)

        renderResources :=
            Some {
                shipRenderer = new InstancedModelRenderer(gdm, ship, InstancingTechnique = instancingTechnique)
                asteroidRenderer = new InstancedModelRenderer(gdm, asteroid, InstancingTechnique = instancingTechnique)        
                shipModel = ship
                asteroidModel = asteroid
                spriteBatch = sb
                font = courier
                effect = fx
                radar = radar
                radarTexture = radarTexture
                dot = dotTexture
                marker = marker
                circle = circle
                log = log
            }

    let disposeAll() =
        content.Unload()
        match renderResources.Value with
        | Some r ->
            r.spriteBatch.Dispose()
            r.effect.Dispose()
        | None -> ()
        dispose gdm

    let update (gt : GameTime) (state : State, computationTime) =
        session.Update()
        match renderResources.Value with
        | Some { log = log } -> log.Update(1.0f<s> * (gt.ElapsedGameTime.TotalSeconds |> float32))
        | None -> ()

        let playerIndices =
            let rec work playerIndices localPlayers =
                match playerIndices, localPlayers with
                | _, [] -> []
                | [], player :: players -> None :: work [] players
                | idx :: idxs, player :: players ->
                    if state.players.localAiPlayerIdxs |> List.exists ((=) player) then
                        None :: work playerIndices players
                    else
                        Some idx :: work idxs players
            work [PlayerIndex.One; PlayerIndex.Two; PlayerIndex.Three; PlayerIndex.Four] state.players.localPlayersIdxs

        let settings =
            playerIndices
            |> List.map (fun _ -> ShipControl.defaultSettings)

        let humanControls =
            ShipControl.getAllControls settings playerIndices

        let messages =
            let reader = new PacketReader()
            [
                let keepGoing = ref true
                while keepGoing.Value do
                    match Network.receive session reader with
                    | Some(_, sender) ->
                        match Network.readTimedRemoteEvent reader with
                        | { event = DamageAndImpulse _ }
                        | { event = BulletFired _ }
                        | { event = BulletDestroyed _ }
                        | { event = ShipState _ } as v -> // Ignore bullet and ship updates that come from us.
                            if not sender.IsLocal then
                                yield v
                        | v -> yield v
                    | None -> keepGoing := false
            ]

        // Fast-forward the clock if someone else's clock is ahead of ours.
        let time =
            messages
            |> List.map (fun msg -> msg.time)
            |> List.append [state.time]
            |> List.max

        let state = { state with time = time }

        let renderData =
            computationTime,
            [
                for gamer in session.LocalGamers do
                    let id = 1<LivePlayer> * int gamer.Id
                    match participants.GlobalPlayerIndexOfLivePlayer id with
                    | Some subject when int subject < state.players.numPlayers ->
                        yield
                            (state.ships.posHost.[subject],
                             state.ships.headings.[subject],
                             state.ships.rights.[subject],
                             state.ships.speeds.[subject],
                             state.bullets.pos,
                             state.bullets.radii,
                             state.ships.posVisible.Content,
                             state.ships.speeds.Content,
                             state.ships.headings.Content,
                             state.ships.rights.Content,
                             state.players.shipTypes.Content
                            )
                    | Some _ -> () // player not yet registered in the state.
                    | None -> ()
            ]

        renderData
        ,
        (state, humanControls, messages)

    let watch = new System.Diagnostics.Stopwatch()

    let compute (gt : GameTime) (state : GameState.State, humanControls, messages) =
        let dt = 1.0f<s> * (gt.ElapsedGameTime.TotalSeconds |> float32)

        watch.Reset()
        watch.Start()

        let aiStates, aiControls =
            List.zip state.players.localAiPlayerIdxs state.ais
            |> List.map (fun (idxAi, ai) -> AiSteering.updateAi (getBulletSpeed state.players) dt state ai idxAi)
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
            work humanControls aiControls state.players.localPlayersIdxs state.players.localAiPlayerIdxs

        // Firing
        let players, newBullets, bulletCount =
            ShipControl.fireBullets state.bullets.bulletCounter state.players state.ships controls

        let bulletEvents =
            newBullets
            |> List.map (fun data -> { time = state.time; event = RemoteEvent.BulletFired data } )

        // Flight assistance
        let headings, rights, targetSpeeds, forces =
            ShipControl.handlePlayerInputs dt players state.ships controls

        // Update local state.
        let state =
            { state with
                players = { players with localTargetSpeeds = targetSpeeds }
                ais = aiStates
                bullets = { state.bullets with bulletCounter = bulletCount } }

        // Add events for localy created bullets
        let events =
            List.append messages bulletEvents
            |> Array.ofList

        // Update simulation of all entities
        let state, messagesOut = GameStateUpdate.update dt events forces headings rights description state

        // Include locally created bullets in the messages to send
        let messagesOut = List.append messagesOut bulletEvents

        // Send messages
        let writer = new PacketWriter()
        for msg in messagesOut do
            Network.writeTimedRemoteEvent writer msg
            let sendOptions =
                match msg.event with
                | PlayerJoined _ | PlayerLeft _
                | BulletFired _ | BulletDestroyed _
                | SupplyDisappeared _ | SupplySpawned _ -> SendDataOptions.ReliableInOrder
                | DamageAndImpulse _
                | ShipDestroyed _ -> SendDataOptions.Reliable
                | ShipState _ -> SendDataOptions.None
                | BuildAsteroids _ -> failwith "Cannot change asteroids field during the game"
        
            Network.broadcast sendOptions session writer

        // Add local players among those that just joined
        let newPlayers =
            messages
            |> List.choose (function
                | { event = PlayerJoined(idx, live, name, pos) } ->
                    Some (idx, live, name, pos)
                | _ -> None)

        for gpi, id, name, _ in newPlayers do
            participants.SetGlobalPlayerIndexOfLivePlayer(id, gpi)
            match renderResources.Value with
            | Some { log = log } -> log.AddMessage(TextIcons.String(sprintf "%s joined" name, TextIcons.Nil))
            | None -> ()

        let localPlayers, numFastBullets, numBigBullets, numHighRate, numMultiFire, timeBeforeFire, timeBeforeRespawn, targetSpeeds =
            newPlayers
            |> List.fold (fun ((localPlayers, numFastBullets, numBigBullets, numHighRate, numMultiFire, timeBeforeFire, timeBeforeRespawn, targetSpeeds) as lists) (idx, live, _, _) ->
                if session.AllGamers
                   |> Seq.exists (fun gamer -> 1<LivePlayer> * int gamer.Id = live && gamer.IsLocal) then
                    idx :: localPlayers,
                    0 :: numFastBullets,
                    0 :: numBigBullets,
                    0 :: numHighRate,
                    0 :: numMultiFire,
                    0<dms> :: timeBeforeFire,
                    -1<dms> :: timeBeforeRespawn,
                    0.0f<m/s> :: targetSpeeds
                else
                    lists)
                (state.players.localPlayersIdxs,
                 state.players.numFastBullets,
                 state.players.numBigBullets,
                 state.players.numHighRate,
                 state.players.numMultiFire,
                 state.players.timeBeforeFire,
                 state.players.timeBeforeRespawn,
                 state.players.localTargetSpeeds)

        let players =
            newPlayers            
            |> Seq.fold (fun players (idx, _, name, pos) -> addPlayer idx name players) state.players

        // Mark those that have been removed.
        let players =
            messages
            |> List.choose (fun ev ->
                match ev with
                | { event = PlayerLeft(idx) } -> Some idx
                | _ -> None)
            |> List.fold (fun players idx -> removePlayer idx players) players

        // A dummy set of values for ship position and orientation. The correct values are set after the arrays are grown.
        let dummyNewPos : ShipPosition =
            { position = TypedVector3<m>(Vector3.Zero)
              heading = TypedVector3<1>(-Vector3.UnitZ)
              right = TypedVector3<1>(Vector3.UnitX)
            }

        // Grow the arrays of ship data, if needed.
        let ships =
            if state.players.numPlayers = players.numPlayers then
                state.ships
            else
                seq { state.players.numPlayers .. players.numPlayers - 1 }
                |> Seq.fold (fun ships _ -> addNewShip dummyNewPos ships) state.ships

        // Set the position and orientation of each new ship
        for (idx, liveId, name, pos) in newPlayers do
            ships.posClient.[idx] <- pos.position
            ships.posHost.[idx] <- pos.position
            ships.posVisible.[idx] <- pos.position
            ships.headings.[idx] <- pos.heading
            ships.rights.[idx] <- pos.right

        let state =
            { state with
                players =
                    { players with
                        localPlayersIdxs = localPlayers
                        numFastBullets = numFastBullets
                        numBigBullets = numBigBullets
                        numHighRate = numHighRate
                        numMultiFire = numMultiFire
                        timeBeforeFire = timeBeforeFire
                        timeBeforeRespawn = timeBeforeRespawn 
                        localTargetSpeeds = targetSpeeds }
                ships = ships }

        participants.Update(state.time)

        state, watch.Elapsed

    let renderAsteroids = Rendering.renderAsteroids (1.0f / 200.0f) description.asteroids.pos.Content description.asteroids.rotations.Content description.asteroids.radius.Content description.asteroids.fieldSizes
    
    let draw (gt : GameTime) (computationTime : System.TimeSpan, data) =
        for (pos, heading, right, speed : TypedVector3<m/s>, bulletPos, bulletRadii, shipPos, shipSpeeds, shipHeadings, shipRights, shipTypes) in data do
            match renderResources.Value with
            | Some r ->
                try
                    gdm.GraphicsDevice.SetRenderTarget(r.radar)
                    gdm.GraphicsDevice.Clear(Color.Black)
                    let assets : Rendering.ShipRadar.ShipRadarRenderingAssets =
                        { radar = r.radarTexture
                          dot = r.dot
                        }
                    Rendering.ShipRadar.render assets r.spriteBatch pos heading right shipPos
                finally
                    gdm.GraphicsDevice.SetRenderTarget(null)

                gdm.GraphicsDevice.Clear(Color.Black)

                let saveState = gdm.GraphicsDevice.SamplerStates.[0]
                try
                    gdm.GraphicsDevice.SamplerStates.[0] <- Graphics.SamplerState.LinearWrap
                    renderAsteroids r.asteroidRenderer pos heading right
                finally
                    gdm.GraphicsDevice.SamplerStates.[0] <- saveState

                Rendering.renderBullets
                    gdm.GraphicsDevice
                    r.effect
                    (fun V -> r.effect.View <- V)
                    (fun P -> r.effect.Projection <- P)
                    (fun W -> r.effect.World <- W)
                    bulletPos
                    bulletRadii
                    pos
                    heading
                    right

                Rendering.renderShips
                    r.shipRenderer
                    pos
                    heading
                    right
                    shipPos
                    shipHeadings
                    shipRights
                    shipTypes

                let ww = 1.0f<Screen> * float32 gdm.GraphicsDevice.Viewport.Width
                let hh = 1.0f<Screen> * float32 gdm.GraphicsDevice.Viewport.Height

                TargetingHud.renderTargeting r.spriteBatch r.marker r.circle ww hh Rendering.fieldOfView Rendering.ratio heading right pos speed shipPos shipSpeeds

                try
                    r.spriteBatch.Begin()
                    let tsa = gdm.GraphicsDevice.Viewport.TitleSafeArea
                    r.spriteBatch.Draw(r.radar, Rectangle(tsa.Right - 200, tsa.Bottom - 200, 200, 200), new System.Nullable<_>(Rectangle(0, 0, Rendering.ShipRadar.width, Rendering.ShipRadar.height)), Color.White)
                finally
                    r.spriteBatch.End()

            | _ -> ()

        // Debug info, stats, log
        match renderResources.Value with
        | Some r ->
            try
                r.spriteBatch.Begin()
                r.spriteBatch.DrawString(r.font, sprintf "%3.1f %%" (100.0 * computationTime.TotalSeconds / 0.016667), Vector2(100.0f, 100.0f), Color.White)
                r.log.Render(r.spriteBatch, Color.White, 0.25f, 100.0f, 120.0f)
                let tsa = gdm.GraphicsDevice.Viewport.TitleSafeArea
                r.spriteBatch.Draw(r.radar, Rectangle(tsa.Right - 200, tsa.Bottom - 200, 200, 200), new System.Nullable<_>(Rectangle(0, 0, Rendering.ShipRadar.width, Rendering.ShipRadar.height)), Color.White)
            finally
                r.spriteBatch.End()

        | None -> ()


    let comp = new ParallelUpdateDrawGameComponent<_, _, _>(game, (initialState, System.TimeSpan.FromTicks(0L)), initialize, update, compute, draw, disposeAll)
    comp.Disposed.Add <| fun _ ->
        participants.Dispose()
    comp


let setup (game : Game, playerIndices : PlayerIndex seq) =
    let playerIndices = List.ofSeq playerIndices
    let scheduler = new Scheduler()
    let sys = new Environment(scheduler)
    let initData = ref None
    scheduler.AddTask
        (task {
            let numSignInSlots =
                [1; 2; 4]
                |> Seq.find(fun n -> n >= List.length playerIndices)

            let rec signIn = task {
                let notSignedInPlayers =
                    playerIndices
                    |> List.filter (fun (pi : PlayerIndex) -> GamerServices.Gamer.SignedInGamers.ItemOpt(pi).IsNone)
                if not <| List.isEmpty notSignedInPlayers then
                    do! StorageTasks.doOnGuide (fun () ->
                        let indices =
                            notSignedInPlayers
                            |> List.fold (fun s pi -> sprintf "%s [%d]" s (int pi)) ""
                        StorageTasks.info (sprintf "The following controllers must sign in: %s" indices))
                    do! StorageTasks.doOnGuide (fun () -> GamerServices.Guide.ShowSignIn(numSignInSlots, false))
                    do! sys.WaitUntil(fun () -> not GamerServices.Guide.IsVisible)
                    return! signIn
                else
                    return ()
            }
            do! signIn

            let! data = Network.start sys NetworkSessionType.SystemLink
            for pi in playerIndices do
                match GamerServices.Gamer.SignedInGamers.ItemOpt(pi) with
                | Some gamer ->
                    let session, _, _, _, _ = data
                    let hasJoined =
                        session.LocalGamers
                        |> Seq.exists (fun localGamer -> localGamer.SignedInGamer = gamer)                        
                    if not hasJoined then
                        session.AddLocalGamer(gamer)
                | None -> failwithf "Player %d not signed in" (int pi)
            initData := Some data
        })

    let initiatingComponent =
        { new GameComponent(game) with
            member this.Update(gt) =
                let dt = gt.ElapsedGameTime.TotalSeconds |> float32
                scheduler.RunFor dt
                if not scheduler.HasLiveTasks then
                    match initData.Value with
                    | Some (session, seed, size, random, description) ->
                        let initialState = emptyState 0
                        game.Components.Remove(this) |> ignore
                        game.Components.Add(newComponent (game, description, initialState, session, seed, size, random))
                    | None -> failwith "Failed to create or join session"
        }

    game.Components.Add(initiatingComponent)
