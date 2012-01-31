module SpaceArena2100.Network

open System
open System.Collections.Generic

open Microsoft.Xna.Framework
open Microsoft.Xna.Framework.Net

open CleverRake.XnaUtils
open CleverRake.XnaUtils.CoopMultiTasking
open CleverRake.XnaUtils.Random

open GameState
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


let writeIntList (writer : PacketWriter) (xs : int<_> list) =
    xs |> List.length |> writer.Write
    xs |> List.iter (fun x -> x |> int |> writer.Write)


let readIntList<[<Measure>]'M> (reader : PacketReader) =
    let n = reader.ReadInt32()
    [ for i in 1 .. n -> LanguagePrimitives.Int32WithMeasure<'M>(reader.ReadInt32()) ]


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
      myHostId = 1;
      playerNames = MarkedArray [||];
      localPlayersIdxs = [];
      localAiPlayerIdxs = [];
      shipTypes = MarkedArray [||];
      gonePlayerIdxs = [];
      asteroids = asteroids;
    }


/// Units of numerical ids assigned by the XNA framework.
type [<Measure>] LivePlayer

let addNewShip (random : System.Random) ships =
    let pos = TypedVector3<m>(random.NextVector3(100.0f))
    let orientation = random.NextQuaternion()
    let heading = TypedVector3<1>(Vector3.Transform(Vector3.UnitY, orientation))
    let right = TypedVector3<1>(Vector3.Transform(Vector3.UnitX, orientation))

    let ships : Ships =
        { accels = MarkedArray.add (TypedVector3<m/s^2>()) ships.accels
          headings = MarkedArray.add heading ships.headings
          rights = MarkedArray.add right ships.rights
          posHost = MarkedArray.add pos ships.posHost
          posVisible = MarkedArray.add pos ships.posVisible
          posClient = MarkedArray.add pos ships.posClient
          posLerpT = MarkedArray.add 1.0f ships.posLerpT
          speeds = MarkedArray.add (TypedVector3<m/s>()) ships.speeds
          health = MarkedArray.add 1.0f<Health> ships.health
          numFastBullets = 0 :: ships.numFastBullets
          numBigBullets = 0 :: ships.numBigBullets
          numMultiFire = 0 :: ships.numMultiFire
          numHighRate = 0 :: ships.numHighRate
          timeBeforeFire = 0<dms> :: ships.timeBeforeFire
          timeBeforeRespawn = -1<dms> :: ships.timeBeforeRespawn
          localTargetSpeeds = 0.0f<m/s> :: ships.localTargetSpeeds
          scores = MarkedArray.add 0.0f<Points> ships.scores
        }

    ships


let addLocalPlayer (random : System.Random) (gamer : LocalNetworkGamer) (description, mapping, state : GameState.State) =
    let newGPI = description.numPlayers * 1<GPI>
    let idLive = (int gamer.Id) * 1<LivePlayer>
    let mapping = Map.add idLive newGPI mapping
    let description =
        { description with
            numPlayers = description.numPlayers + 1
            playerNames = MarkedArray.add gamer.Gamertag description.playerNames
            localPlayersIdxs = newGPI :: description.localPlayersIdxs        
        }
    let ships = addNewShip random state.ships
    let state = { state with ships = ships }
    (description, mapping, state)


let addRemotePlayer (random : System.Random) (gamer : NetworkGamer) (description, mapping, state : GameState.State) =
    let newGPI = description.numPlayers * 1<GPI>
    let idLive = (int gamer.Id) * 1<LivePlayer>
    let mapping = Map.add idLive newGPI mapping
    let description =
        { description with
            numPlayers = description.numPlayers + 1
            playerNames = MarkedArray.add gamer.Gamertag description.playerNames
        }
    let ships = addNewShip random state.ships
    let state = { state with ships = ships }
    (description, mapping, state)


let addLocalAiPlayer (random : System.Random) (description, state) =
    let newGPI = description.numPlayers * 1<GPI>
    let description =
        { description with
            numPlayers = description.numPlayers + 1
            playerNames = MarkedArray.add (sprintf "HAL%d" (9000 + int newGPI)) description.playerNames
            localPlayersIdxs = newGPI :: description.localPlayersIdxs
            localAiPlayerIdxs = newGPI :: description.localAiPlayerIdxs
        }
    let ais = GameState.AiState.Undecided :: state.ais
    let ships = addNewShip random state.ships
    let state = { state with ships = ships ; ais = ais }
    (description, state)


let removePlayer (gamer : NetworkGamer) map description =
    match Map.tryFind ((int gamer.Id) * 1<LivePlayer>) map with
    | Some idx ->
        let description =
            { description with
                gonePlayerIdxs = idx :: description.gonePlayerIdxs
                localPlayersIdxs = description.localPlayersIdxs |> List.filter ((<>) idx) }
        description
    | None -> failwith "No player with that Live id"
            

type Server(sys, sessionType) =
    let seed = int32 <| System.DateTime.Now.Ticks % (1L + int64 Int32.MaxValue)
    let random = new System.Random(seed)

    let description = newDescription random
    let newGamers = ref []
    let removedGamers = ref []
    let packetWriter = new PacketWriter()
    let mapping = ref Map.empty
    let session = ref None

    member this.Run =
        task {
            let! s = newSession sys sessionType
            session := Some s
            s.GamerJoined.Add (fun gamerJoined -> newGamers := gamerJoined.Gamer :: newGamers.Value)
            s.GamerLeft.Add (fun gamerLeft -> removedGamers := gamerLeft.Gamer :: removedGamers.Value)
            return (s, description)
        }

    member this.Update(description, state) =
        match session.Value with
        | Some session ->
            let description, m, state =
                newGamers.Value
                |> List.fold (fun dms gamer ->
                    match gamer with
                    | :? LocalNetworkGamer as gamer ->
                        addLocalPlayer random gamer dms
                    | _ ->
                        packetWriter.Write(seed)
                        send SendDataOptions.ReliableInOrder session gamer packetWriter
                        addRemotePlayer random gamer dms
                    )
                    (description, mapping.Value, state)
            let description =
                removedGamers.Value
                |> List.fold (fun d gamer -> removePlayer gamer m d) description
            newGamers := []
            removedGamers := []
            mapping := m
            (description, state)
        | None ->
            (description, state)