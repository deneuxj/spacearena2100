module SpaceArena2100.Network

open System
open System.Collections.Generic

open Microsoft.Xna.Framework
open Microsoft.Xna.Framework.Net

open CleverRake.XnaUtils
open CleverRake.XnaUtils.CoopMultiTasking

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


let newDescription (random : System.Random) playerNames (isLocal : bool[]) numHosts numAis =
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

    let numHumanPlayers = Array.length playerNames
    let isLocal : MarkedArray<GPI, bool> = MarkedArray isLocal
    let localPlayerIndexes =
        [
            for idx in 0<GPI> .. 1<GPI> .. 1<GPI> * (numHumanPlayers - 1) do
                if isLocal.[idx] then
                    yield idx
        ]

    let numPlayers = numAis + numHumanPlayers

    let rec assignToHost numHosts host idx =
        seq {
            if int idx < numAis then
                yield host, 1<GPI> * (numHumanPlayers + idx)
                yield! assignToHost numHosts (1 + (host + 1) % numHosts) (idx + 1)
        }

    let aisAssignment =
        assignToHost numHosts 1 0
        |> Seq.toList
    
    let myHostId = 1
    
    let localAiPlayerIdxs =
        aisAssignment
        |> List.choose(fun (host, idx) -> if host = myHostId then Some idx else None)

    let playerNames =
        Array.append
            playerNames
            (Array.init numAis (fun i -> sprintf "HAL%d" (9000 + i)))

    { numPlayers = numPlayers;
      myHostId = myHostId;
      playerNames = MarkedArray playerNames;
      localPlayersIdxs = localPlayerIndexes;
      localAiPlayerIdxs = localAiPlayerIdxs;
      shipTypes = MarkedArray (Array.create numPlayers Bull);
      gonePlayerIdxs = [];
      asteroids = asteroids;
    }
    ,
    aisAssignment


let sendAsteroids (writer : PacketWriter)(asteroids : Asteroids) =
    asteroids.fieldSizes.v |> writer.Write
    asteroids.pos.Content.Length |> writer.Write
    asteroids.pos.Content |> Seq.iter (fun pos -> writer.Write pos.v)
    asteroids.radius.Content |> Seq.iter (fun r -> writer.Write (float32 r))
    asteroids.rotations.Content |> Seq.iter (fun rot -> writer.Write rot)


let receiveAsteroids (reader : PacketReader) =
    let V = TypedVector3<m>(reader.ReadVector3())
    let pos =
        let n = reader.ReadInt32()
        [| for i in 1 .. n -> TypedVector3<m>(reader.ReadVector3()) |]
        |> MarkedArray
    let radii =
        let n = reader.ReadInt32()
        [| for i in 1 .. n -> 1.0f<m> * reader.ReadSingle() |]
        |> MarkedArray
    let octree = buildOctree V.X pos radii
    let rotations =
        let n = reader.ReadInt32()
        [| for i in 1 .. n -> reader.ReadQuaternion() |]
        |> MarkedArray
    { pos = pos
      radius = radii
      rotations = rotations
      octree = Octree.freeze octree
      fieldSizes = V }


let sendIntList (writer : PacketWriter) (xs : int<_> list) =
    xs |> List.length |> writer.Write
    xs |> List.iter (fun x -> x |> int |> writer.Write)


let receiveIntList<[<Measure>]'M> (reader : PacketReader) =
    let n = reader.ReadInt32()
    [ for i in 1 .. n -> LanguagePrimitives.Int32WithMeasure<'M>(reader.ReadInt32()) ]


let sendDescription (writer : PacketWriter) (description : Description) =
    let d = description
    d.asteroids |> sendAsteroids writer
    d.gonePlayerIdxs |> sendIntList writer
    // Don't send d.localAiPlayerIdxs -> handled by ai player distribution
    // Don't send d.localPlayersIdxs -> can be retrieved from NetworkSession.AllGamers and NetworkSession.LocalGamers.
    // Don't send d.myHostId -> handled by player distribution
    d.numPlayers |> writer.Write
    // d.playerNames.Content.Length |> writer.Write -> No need, same as numPlayers
    d.playerNames.Content |> Seq.iter writer.Write
    d.shipTypes.Content |> Seq.iter (fun x -> x.ToInt() |> writer.Write)


let distributePlayers (gamers : NetworkGamer seq) =
    let mutable nextHostId = 1
    let hostIdOfHost = new System.Collections.Generic.Dictionary<NetworkMachine, int>()
    for gamer in gamers do
        match hostIdOfHost.TryGetValue(gamer.Machine) with
        | false, _ ->
            let hostId = nextHostId
            hostIdOfHost.[gamer.Machine] <- hostId
            nextHostId <- nextHostId + 1
        | true, _ -> ()

    gamers
    |> Seq.map (fun gamer -> hostIdOfHost.[gamer.Machine])
    |> Seq.toArray


let sendPlayerDistribution (writer : PacketWriter) (hostOfPlayer : int[]) =
    hostOfPlayer.Length |> writer.Write
    hostOfPlayer |> Array.iter writer.Write


let receivePlayerDistribution (reader : PacketReader) =
    let n = reader.ReadInt32()
    [| for i in 1 .. n -> reader.ReadInt32() |]


let sendAiPlayerDistribution (writer : PacketWriter) (dist : (int * int<GPI>) list) =
    dist |> List.length |> writer.Write
    dist |> List.iter (fun (ai, idx) -> writer.Write(ai); writer.Write(int idx))


let receiveAiPlayerDistribution (reader : PacketReader) =
    let n = reader.ReadInt32()
    [ for i in 1 .. n -> (reader.ReadInt32(), LanguagePrimitives.Int32WithMeasure<GPI>(reader.ReadInt32())) ]


let receiveDescription (reader : PacketReader) =
    let asteroids = receiveAsteroids reader
    let gonePlayerIdxs = receiveIntList<GPI> reader
    let numPlayers = reader.ReadInt32()
    let playerNames =
        MarkedArray [| for i in 1 .. numPlayers -> reader.ReadString() |]
    let shipTypes =
        MarkedArray [| for i in 1 .. numPlayers -> reader.ReadInt32() |> ShipType.FromInt |]

    let playerDist = receivePlayerDistribution reader
    let aiPlayerDist = receiveAiPlayerDistribution reader

    failwith "TODO"


let sendTypedVector3 (writer : PacketWriter) (v : TypedVector3<'M>) =
    writer.Write(v.v)


let receiveTypedVector3<[<Measure>]'M> (reader : PacketReader) =
    new TypedVector3<'M>(reader.ReadVector3())


let makeRandomInitialStateData (rnd : System.Random) numPlayers =
    let d = 300.0f<m>
    let rndVec() =
        let x = rnd.NextDouble() |> float32
        let y = rnd.NextDouble() |> float32
        let z = rnd.NextDouble() |> float32
        if rnd.Next(2) = 0 then
            -1.0f
        else
            1.0f
        *
        Vector3(x, y, z)

    let zipped =
        [|
            for i in 1 .. numPlayers do                       
                let pos = d * TypedVector.normalize3(TypedVector3<m>(rndVec()))
                let heading = TypedVector.normalize3(TypedVector3<m>(rndVec()))
                let v = TypedVector3<m>(rndVec())
                let right = TypedVector.cross3(v, heading) |> TypedVector.normalize3
                yield (pos, heading, right)
        |]

    Array.unzip3 zipped
    
        
let sendInitialStates (writer : PacketWriter) (pos : TypedVector3<m>[]) (headings : TypedVector3<1>[]) (rights : TypedVector3<1>[]) =
    let n = Array.length pos
    pos |> Array.iter (sendTypedVector3 writer)
    headings |> Array.iter (sendTypedVector3 writer)
    rights |> Array.iter (sendTypedVector3 writer)


let receiveInitialState (reader : PacketReader) numLocalPlayers numLocalAis myHostId =
    let n = reader.ReadInt32()
    let pos = MarkedArray [| for i in 1 .. n -> receiveTypedVector3<m> reader |]
    let headings = MarkedArray [| for i in 1 .. n -> receiveTypedVector3<1> reader |]
    let rights = MarkedArray [| for i in 1 .. n -> receiveTypedVector3<1> reader |]

    let ships =
        { headings = headings
          rights = rights
          posClient = pos
          posVisible = pos
          posHost = pos
          posLerpT = MarkedArray (Array.zeroCreate n)
          speeds = MarkedArray (Array.zeroCreate n)
          accels = MarkedArray (Array.zeroCreate n)
          health = MarkedArray (Array.create n 1.0f<Health>)
          numFastBullets = List.init numLocalPlayers (fun _ -> 0)
          numBigBullets = List.init numLocalPlayers (fun _ -> 0)
          numMultiFire = List.init numLocalPlayers (fun _ -> 0)
          numHighRate = List.init numLocalPlayers (fun _ -> 0)
          timeBeforeFire = List.init numLocalPlayers (fun _ -> 0<dms>)
          timeBeforeRespawn = List.init numLocalPlayers (fun _ -> -1<dms>)
          localTargetSpeeds = List.init numLocalPlayers (fun _ -> 0.0f<m/s>)
          scores = MarkedArray (Array.zeroCreate n)
        }

    let ais =
        List.init numLocalAis (fun _ -> Undecided)

    let bullets =
        { guids = [||]
          pos = [||]
          timeLeft = [||]
          speeds = [||]
          radii = [||]
          owners = [||]
          lastLocalGuid = GameStateUpdate.firstGuid myHostId }

    let supplies =
        { pos = MarkedArray [||]
          types = MarkedArray [||]
          radii = MarkedArray [||] }

    { ships = ships
      ais = ais
      bullets = bullets
      supplies = supplies
      time = 0<dms> }


let pumpAll (session : NetworkSession) (reader : PacketReader) =
    for gamer in session.LocalGamers do
        while gamer.IsDataAvailable do
            gamer.ReceiveData(reader) |> ignore

let sendToAllMachines (session : NetworkSession) (writer : PacketWriter) options =
    let machinesAlreadySentTo = new HashSet<_>()

    Maybe.maybe {
        let! sender =
            session.LocalGamers
            |> Seq.tryFind (fun gamer -> gamer.SignedInGamer.Privileges.AllowOnlineSessions)

        for gamer in session.RemoteGamers do
            if not <| machinesAlreadySentTo.Contains(gamer.Machine) then
                machinesAlreadySentTo.Add(gamer.Machine) |> ignore
                sender.SendData(writer, options, gamer)
    }
                
let initiateGame (sys : Environment) (session : NetworkSession) sessionCancelled =
    let random = new System.Random()
    let reliable = SendDataOptions.ReliableInOrder
    let writer = new PacketWriter()
    let reader = new PacketReader()

    task {
        do! sys.WaitUntil(fun () -> session.IsEveryoneReady || sessionCancelled())
        if not (sessionCancelled()) then
            session.StartGame()
            do! sys.Wait(1.0f)

            let hosts =
                session.AllGamers
                |> Seq.map (fun gamer -> gamer.Machine)
                |> Seq.distinct
                |> Seq.toArray
            let numAisPerHost = 5
            let playerNames =
                session.AllGamers
                |> Seq.map (fun gamer -> gamer.Gamertag)
                |> Seq.toArray
            let isLocal =
                session.AllGamers
                |> Seq.map (fun gamer -> gamer.IsLocal)
                |> Seq.toArray

            let description, aiDistribution =
                newDescription random playerNames isLocal hosts.Length (hosts.Length * numAisPerHost)
            let playerDistribution = distributePlayers session.AllGamers
            sendDescription writer description
            sendPlayerDistribution writer playerDistribution
            sendAiPlayerDistribution writer aiDistribution

            let pos, headings, rights = makeRandomInitialStateData random description.numPlayers
            sendInitialStates writer pos headings rights

            let comGamer = session.LocalGamers.[0]
            comGamer.SendData(writer, reliable)
                
            do! sys.WaitUntil(fun () -> comGamer.IsDataAvailable)

            let _, sender = session.LocalGamers.[0].ReceiveData(reader)
            let initialState = receiveInitialState reader (List.length description.localPlayersIdxs) (List.length description.localAiPlayerIdxs) description.myHostId

            return Some(description, initialState)
        else
            return None
    }