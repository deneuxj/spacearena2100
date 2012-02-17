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
    }


let newComponent (game : Game, initialState, session, seed, random, unsubscribe) =
    
    let description, state = initialState

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
            }

    let disposeAll() =
        content.Unload()
        match renderResources.Value with
        | Some r ->
            r.spriteBatch.Dispose()
            r.effect.Dispose()
        | None -> ()
        dispose gdm

    let settings =
        description.localPlayersIdxs
        |> List.map (fun _ -> ShipControl.defaultSettings)

    let update (gt : GameTime) (description : Description, state : State, computationTime) =
        let me = 0<GPI>
        let ai = 1<GPI>

        let playerIndices =
            let rec work playerIndices localPlayers =
                match playerIndices, localPlayers with
                | _, [] -> []
                | [], player :: players -> None :: work [] players
                | idx :: idxs, player :: players ->
                    if description.localAiPlayerIdxs |> List.exists ((=) player) then
                        None :: work playerIndices players
                    else
                        Some idx :: work idxs players
            work [PlayerIndex.One; PlayerIndex.Two; PlayerIndex.Three; PlayerIndex.Four] description.localPlayersIdxs

        let humanControls =
            ShipControl.getAllControls settings playerIndices

        let subject = me
        (state.ships.posHost.[subject],
         state.ships.headings.[subject],
         state.ships.rights.[subject],
         state.ships.speeds.[subject],
         computationTime,
         state.bullets.pos,
         state.bullets.radii,
         state.ships.posVisible.Content,
         state.ships.speeds.Content,
         state.ships.headings.Content,
         state.ships.rights.Content,
         description.shipTypes.Content
        )
        ,
        (description, state, humanControls)

    let watch = new System.Diagnostics.Stopwatch()

    let compute (gt : GameTime) (description : Description, state : GameState.State, humanControls) =
        let dt = 1.0f<s> * (gt.ElapsedGameTime.TotalSeconds |> float32)

        watch.Reset()
        watch.Start()

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

        let ships, newBullets, bulletCount =
            ShipControl.fireBullets state.bullets.bulletCounter description.localPlayersIdxs state.ships controls

        let controls =
            ShipControl.handlePlayerInputs dt description.localPlayersIdxs ships description.shipTypes controls

        let headings, rights, targetSpeeds, forces = controls

        let state =
            { state with
                ships = ships
                ais = aiStates
                bullets = { state.bullets with bulletCounter = bulletCount } }

        let timedEvents =
            newBullets
            |> List.map (fun data -> { time = state.time; event = RemoteEvent.BulletFired data } )
            |> Array.ofList

        let state =
            { state with
                ships = { state.ships with localTargetSpeeds = targetSpeeds } }
        let dt = 1.0f<s> * (gt.ElapsedGameTime.TotalSeconds |> float32)
        let description', state' = GameStateUpdate.update dt timedEvents forces headings rights (description, state)

        description', state', watch.Elapsed

    let renderAsteroids = Rendering.renderAsteroids (1.0f / 200.0f) description.asteroids.pos.Content description.asteroids.rotations.Content description.asteroids.radius.Content description.asteroids.fieldSizes
    
    let draw (gt : GameTime) (pos, heading, right, speed : TypedVector3<m/s>, computationTime : System.TimeSpan, bulletPos, bulletRadii, shipPos, shipSpeeds, shipHeadings, shipRights, shipTypes) =
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
                r.spriteBatch.DrawString(r.font, sprintf "%3.1f %%" (100.0 * computationTime.TotalSeconds / 0.016667), Vector2(100.0f, 100.0f), Color.White)
                r.spriteBatch.DrawString(r.font, sprintf "%4.2f" (TypedVector.dot3(speed, heading) |> float32), Vector2(130.0f, 220.0f), Color.White)
                let tsa = gdm.GraphicsDevice.Viewport.TitleSafeArea
                r.spriteBatch.Draw(r.radar, Rectangle(tsa.Right - 200, tsa.Bottom - 200, 200, 200), new System.Nullable<_>(Rectangle(0, 0, Rendering.ShipRadar.width, Rendering.ShipRadar.height)), Color.White)
            finally
                r.spriteBatch.End()
        | _ -> ()

    let participants = new Network.Participants(session, seed, random, unsubscribe)
    let participantsWrapper =
        { new GameComponent(game) with
            member this.Update(_) = participants.Update()            
        }
    game.Components.Add participantsWrapper

    let comp = new ParallelUpdateDrawGameComponent<_, _, _>(game, (description, state, System.TimeSpan.FromTicks(0L)), initialize, update, compute, draw, disposeAll)
    comp.Disposed.Add <| fun _ ->
        unsubscribe()
        game.Components.Remove(participantsWrapper) |> ignore
        participants.Dispose()
    comp


let setup (game : Game, playerIndices) =
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
                let somePlayersAreNotSignedIn =
                    playerIndices
                    |> List.exists (fun (pi : PlayerIndex) -> GamerServices.Gamer.SignedInGamers.ItemOpt(pi).IsNone)
                if somePlayersAreNotSignedIn then
                    do! StorageTasks.doOnGuide (fun () -> GamerServices.Guide.ShowSignIn(numSignInSlots, true))
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
                    | Some (session, seed, random, description, unsubscribe) ->
                        let initialState = description, emptyState 0
                        game.Components.Remove(this) |> ignore
                        game.Components.Add(newComponent (game, initialState, session, seed, random, unsubscribe))
                    | None -> failwith "Failed to create or join session"
        }

    game.Components.Add(initiatingComponent)
