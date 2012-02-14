module SpaceArena2100.Network

open System
open System.Collections.Generic

open Microsoft.Xna.Framework
open Microsoft.Xna.Framework.Net

open CleverRake.XnaUtils
open CleverRake.XnaUtils.CoopMultiTasking
open CleverRake.XnaUtils.Random

open GameState
open GameStateUpdate
open Units

let newSession (sys : Environment) sessionType =
    task {
        let sync = NetworkSession.BeginCreate(sessionType, 4, 31, null, null)
        do! sys.WaitUntil(fun () -> sync.IsCompleted)
        return NetworkSession.EndCreate(sync)
    }

let findAnySession (sys : Environment) sessionType =
    task {
        let sync = NetworkSession.BeginFind(sessionType, 4, null, null, null)
        do! sys.WaitUntil(fun () -> sync.IsCompleted)
        return NetworkSession.EndFind(sync)
    }

let joinSession (sys : Environment) session =
    task {
        let sync = NetworkSession.BeginJoin(session, null, null)
        do! sys.WaitUntil(fun () -> sync.IsCompleted)
        return NetworkSession.EndJoin(sync)
    }


let writeTypedVector3 (writer : PacketWriter) (v : TypedVector3<'M>) =
    writer.Write(v.v)


let readTypedVector3<[<Measure>]'M> (reader : PacketReader) =
    new TypedVector3<'M>(reader.ReadVector3())


let writeTypedInt (writer : PacketWriter) (v : int<_>) =
    writer.Write(int v)


let readTypedInt<[<Measure>]'M> (reader : PacketReader) =
    reader.ReadInt32()
    |> LanguagePrimitives.Int32WithMeasure<'M>


let writeTypedFloat (writer : PacketWriter) (v : float32<_>) =
    writer.Write(int v)


let readTypedFloat<[<Measure>]'M> (reader : PacketReader) =
    reader.ReadSingle()
    |> LanguagePrimitives.Float32WithMeasure<'M>


let writeIntList (writer : PacketWriter) (xs : int<_> list) =
    xs |> List.length |> writer.Write
    xs |> List.iter (fun x -> x |> int |> writer.Write)


let readIntList<[<Measure>]'M> (reader : PacketReader) =
    let n = reader.ReadInt32()
    [ for i in 1 .. n -> LanguagePrimitives.Int32WithMeasure<'M>(reader.ReadInt32()) ]


let writePosition (writer : PacketWriter) pos =
    writeTypedVector3 writer pos.position
    writeTypedVector3 writer pos.heading
    writeTypedVector3 writer pos.right


let readPosition (reader : PacketReader) =
    { position = readTypedVector3 reader
      heading = readTypedVector3 reader
      right = readTypedVector3 reader }


let writeSupplyType (writer : PacketWriter) supType =
    match supType with
    | FastBullets -> writer.Write(0uy)
    | BigBullets -> writer.Write(1uy)
    | HighRate -> writer.Write(2uy)
    | MultiFire -> writer.Write(3uy)


let readSupplyType (reader : PacketReader) =
    match reader.ReadByte() with
    | 0uy -> FastBullets
    | 1uy -> BigBullets
    | 2uy -> HighRate
    | 3uy -> MultiFire
    | _ -> failwith "Unrecognized value for a supply type"


let writeRemoteEvent (writer : PacketWriter) ev =
    match ev with
    | DamageAndImpulse(idx, damage, impulse) ->
        writer.Write(0uy)
        writeTypedInt writer idx
        writeTypedFloat writer damage
        writeTypedVector3 writer impulse
    | BulletDestroyed(idx) ->
        writer.Write(1uy)
        writeTypedInt writer idx
    | ShipState(idx, pos, speed, thrust) ->
        writer.Write(2uy)
        writeTypedInt writer idx
        writePosition writer pos
        writeTypedVector3 writer speed
        writeTypedVector3 writer thrust
    | ShipDestroyed(idx) ->
        writer.Write(3uy)
        writeTypedInt writer idx
    | BulletFired(idx, owner, radius, pos, speed) ->
        writer.Write(4uy)
        writeTypedInt writer idx
        writeTypedInt writer owner
        writeTypedFloat writer radius
        writeTypedVector3 writer pos
        writeTypedVector3 writer speed
    | SupplySpawned(idx, timeLeft, pos, radius, supType) ->
        writer.Write(5uy)
        writeTypedInt writer idx
        writeTypedVector3 writer pos
        writeTypedFloat writer radius
        writeSupplyType writer supType
        writeTypedInt writer timeLeft
    | SupplyDisappeared(idx) ->
        writer.Write(6uy)
        writeTypedInt writer idx
    | PlayerJoined(idx, name, pos) ->
        writer.Write(7uy)
        writeTypedInt writer idx
        writer.Write(name)
        writePosition writer pos
    | PlayerLeft(idx) ->
        writer.Write(8uy)
        writeTypedInt writer idx


let readRemoteEvent (reader : PacketReader) =
    match reader.ReadByte() with
    | 0uy ->
        let idx = readTypedInt reader 
        let damage = readTypedFloat reader
        let impulse = readTypedVector3 reader
        DamageAndImpulse(idx, damage, impulse)
    | 1uy -> 
        let idx = readTypedInt reader
        BulletDestroyed(idx)
    | 2uy ->        
        let idx = readTypedInt reader
        let pos = readPosition reader
        let speed = readTypedVector3 reader
        let thrust = readTypedVector3 reader
        ShipState(idx, pos, speed, thrust)

    | 3uy ->
        let idx = readTypedInt reader
        ShipDestroyed(idx)
    | 4uy ->
        let idx = readTypedInt reader
        let owner =readTypedInt reader
        let radius = readTypedFloat reader
        let pos = readTypedVector3 reader
        let speed = readTypedVector3 reader
        BulletFired(idx, owner, radius, pos, speed)
    | 5uy ->
        let idx = readTypedInt reader
        let pos = readTypedVector3 reader
        let radius = readTypedFloat reader
        let supType = readSupplyType reader
        let timeLeft = readTypedInt reader
        SupplySpawned(idx, timeLeft, pos, radius, supType)
    | 6uy ->
        let idx = readTypedInt reader
        SupplyDisappeared(idx)
    | 7uy ->        
        let idx = readTypedInt reader
        let name = reader.ReadString()
        let pos = readPosition reader
        PlayerJoined(idx, name, pos)
    | 8uy ->
        let idx = readTypedInt reader
        PlayerLeft(idx)
    | _ ->
        failwith "Unexpected value for RemoteEvent"


let writeRemoteEventList (writer : PacketWriter) events =
    writer.Write(List.length events)
    for e in events do
        writeRemoteEvent writer e


let readRemoteEventList (reader : PacketReader) =
    let num = reader.ReadInt32()
    [ for i in 1 .. num do
        yield readRemoteEvent reader ]


let receive (session : NetworkSession) (reader : PacketReader) =
    if session.LocalGamers.Count > 0 then
        let gamer = session.LocalGamers.[0]
        if gamer.IsDataAvailable then
            gamer.ReceiveData(reader)
            |> Some
        else
            let dummyReader = new PacketReader()
            for i in 1 .. session.LocalGamers.Count - 1 do
                session.LocalGamers.[i].ReceiveData(dummyReader) |> ignore
            None
    else
        None


let broadcast options (session : NetworkSession) (writer : PacketWriter) =
    if session.LocalGamers.Count > 0 then
        session.LocalGamers.[0].SendData(writer, options)


let send options (session : NetworkSession) (destination : NetworkGamer) (writer : PacketWriter) =
    if session.LocalGamers.Count > 0 then
        session.LocalGamers.[0].SendData(writer, options, destination)


let buildOctree (fieldSize : float32<m>) (pos : MarkedArray<AstIdx, TypedVector3<m>>) (radius : MarkedArray<AstIdx, float32<m>>) =
    let newBoundingSphere (center : TypedVector3<m>) (radius : float32<m>) =
        new BoundingSphere(center.v, float32 radius)

    let boundingSpheres =
        Array.map2 newBoundingSphere pos.Content radius.Content
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

    octree


let newDescription (random : System.Random) =
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

    let rotations =
        Array.init numAsteroids (fun _ -> randomRotation())
        |> MarkedArray

    let boundingSpace =
        new BoundingBox(float32 fieldSize / 2.0f * -Vector3.One, float32 fieldSize / 2.0f * Vector3.One)

    let octree = buildOctree fieldSize pos radius

    let asteroids : Asteroids =
        { pos = pos;
          radius = radius;
          rotations = rotations;
          octree = Octree.freeze octree;
          fieldSizes = fieldSize * TypedVector3<1>(Vector3.One);
        }
        
    { numPlayers = 0;
      playerNames = MarkedArray [||];
      localPlayersIdxs = [];
      localAiPlayerIdxs = [];
      shipTypes = MarkedArray [||];
      gonePlayerIdxs = [];
      asteroids = asteroids;
    }


let removePlayer (gamer : NetworkGamer) map description =
    match Map.tryFind ((int gamer.Id) * 1<LivePlayer>) map with
    | Some idx -> removePlayerFromDescription idx description
    | None -> failwith "No player with that Live id"
            
type SessionMode = Server | Client

let tryFindSession (sys : Environment) sessionType =
    let rnd = new Random()

    task {
        let attemptDuration = float32 <| rnd.Next(15)
        let attemptTimer = sys.NewStopwatch()
        attemptTimer.Start()
        let session = ref None
        while attemptTimer.ElapsedSeconds < attemptDuration && session.Value.IsNone do
            let! sessions = findAnySession sys sessionType
            if sessions.Count > 0 then
                session := Some sessions.[0]
        return session.Value
    }

let joinOrStart (sys : Environment) sessionType =
    task {
        let! session = tryFindSession sys sessionType
        let mode =
            match session with
            | None -> Server
            | Some _ -> Client

        let! session =
            match session with
            | None -> newSession sys sessionType
            | Some s -> joinSession sys s

        match mode with
        | Server ->
            session.AllowHostMigration <- true
            session.AllowJoinInProgress <- true
        | Client -> ()
        return mode, session
    }

let waitUntilLocalPlayerJoined (sys : Environment) (session : NetworkSession) =
    task {
        let localPlayer = ref None
        while localPlayer.Value.IsNone do
            let! newPlayer = sys.AwaitEvent(session.GamerJoined)
            if newPlayer.Gamer.IsLocal then
                localPlayer := Some <| (newPlayer.Gamer :?> LocalNetworkGamer)
        return localPlayer
    }

let sendSeed session (seed : int32) (gamerJoined : GamerJoinedEventArgs) =
    let packetWriter = new PacketWriter()
    packetWriter.Write(seed)
    send SendDataOptions.ReliableInOrder session gamerJoined.Gamer packetWriter

let startServer (sys : Environment) (session : NetworkSession) (seed : int32) =
    task {
        let! _ = waitUntilLocalPlayerJoined sys session
        let playerJoinedSubscription =
            session.GamerJoined.Subscribe(
                fun (gamerJoined : GamerJoinedEventArgs) ->
                    if not <| gamerJoined.Gamer.IsLocal then sendSeed session seed gamerJoined
            )
        return playerJoinedSubscription
    }

let start sys sessionType =
    task {
        let! mode, session = joinOrStart sys sessionType
        let! seed, random, description, unsubscribe =
            match mode with
            | Server ->
                task {
                    let seed = int32 <| System.DateTime.Now.Ticks % (1L + int64 Int32.MaxValue)
                    let random = new System.Random(seed)
                    let description = newDescription random
                    let! subscription = startServer sys session seed
                    return seed, random, description, fun () -> subscription.Dispose()
                }
            | Client ->
                let reader = new PacketReader()
                task {
                    let! _ = waitUntilLocalPlayerJoined sys session
                    let seed = ref None
                    while seed.Value.IsNone do
                        match receive session reader with
                        | None -> ()
                        | Some _ -> seed := Some <| reader.ReadInt32()
                    let random = new System.Random(seed.Value.Value)
                    let description = newDescription random
                    return seed.Value.Value, random, description, fun () -> ()
                }
        return session, seed, random, description, unsubscribe
    }

type Participants(session : NetworkSession, seed, random : System.Random, unsubscribe) =
    let newGamers : NetworkGamer list ref = ref []
    let removedGamers : NetworkGamer list ref = ref []
    let packetWriter = new PacketWriter()
    let mapping = ref Map.empty
    let unsubscribe = ref unsubscribe
    let allMessages = ref []
    let numPlayers = ref 0

    let updateSubscription (hostChanged : HostChangedEventArgs) =
        unsubscribe.Value()
        if hostChanged.NewHost.IsLocal then
            let subscription =
                session.GamerJoined.Subscribe(
                    fun (gamerJoined : GamerJoinedEventArgs) ->
                        if not <| gamerJoined.Gamer.IsLocal then sendSeed session seed gamerJoined)
            unsubscribe := fun () -> subscription.Dispose()
        else
            unsubscribe := fun () -> ()

    let isHost() =
        session.AllGamers
        |> Seq.exists(fun g -> g.IsLocal && g.IsHost)

    
    let gamerJoinedSubscription = session.GamerJoined.Subscribe (fun (gamerJoined : GamerJoinedEventArgs) -> newGamers := gamerJoined.Gamer :: newGamers.Value)
    let gamerLeftSubscription= session.GamerLeft.Subscribe (fun (gamerLeft : GamerLeftEventArgs) -> removedGamers := gamerLeft.Gamer :: removedGamers.Value)
    let hostChangedSubscription = session.HostChanged.Subscribe updateSubscription

    member this.Update() : unit =
        let isHost = isHost()

        if isHost then
            for newPlayer in newGamers.Value do
                writeRemoteEventList packetWriter allMessages.Value
                send SendDataOptions.ReliableInOrder session newPlayer packetWriter

        let messages, mapping' =
            if isHost then
                let newGPIs =
                    newGamers.Value
                    |> List.mapi (fun i _ -> 1<GPI> * (numPlayers.Value + i))

                let mapping =
                    List.zip newGamers.Value newGPIs
                    |> List.fold (fun m (g, gpi) -> Map.add (1<LivePlayer> * int g.Id) gpi m) mapping.Value

                let newShipPositions =
                    newGPIs
                    |> List.map (fun _ ->
                        let pos = TypedVector3<m>(random.NextVector3(100.0f))
                        let orientation = random.NextQuaternion()
                        let heading = TypedVector3<1>(Vector3.Transform(Vector3.UnitY, orientation))
                        let right = TypedVector3<1>(Vector3.Transform(Vector3.UnitX, orientation))
                        { position = pos
                          heading = heading
                          right = right })

                let newNames =
                    newGamers.Value
                    |> Seq.map (fun g -> g.Gamertag)
                    |> List.ofSeq

                let playerAddedMessages =
                    List.zip3 newGPIs newNames newShipPositions
                    |> List.map PlayerJoined

                let playerRemovedMessages =
                    removedGamers.Value
                    |> Seq.choose (fun g -> mapping.TryFind (1<LivePlayer> * int g.Id))
                    |> Seq.map PlayerLeft
                    |> List.ofSeq

                playerAddedMessages @ playerRemovedMessages, mapping
            else
                [], mapping.Value
        
        if isHost then
            writeRemoteEventList packetWriter allMessages.Value
            broadcast SendDataOptions.ReliableInOrder session packetWriter

        numPlayers := numPlayers.Value + newGamers.Value.Length
        newGamers := []
        removedGamers := []
        mapping := mapping'
        allMessages := messages @ allMessages.Value


    member this.HasLocalPlayers =
        session.AllGamers
        |> Seq.exists (fun g -> g.IsLocal)

    member this.AddAiPlayer() = failwith "TODO"
        
    member this.Dispose() =
        unsubscribe.Value()
        hostChangedSubscription.Dispose()
        gamerLeftSubscription.Dispose()
        gamerJoinedSubscription.Dispose()

    interface System.IDisposable with
        member this.Dispose() = this.Dispose()
