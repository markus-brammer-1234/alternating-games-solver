// IT NEED NOT BE PURELY FUNCTIONAL!

module OnTheFlySolver

open System.Collections.Generic

type Player =
    | One
    | Two

let next player =
    match player with
    | One -> Two
    | Two -> One

type Game<'S when 'S: comparison> = ('S -> Set<'S>) * ('S -> Set<'S>) * Set<'S>

type Vertex = int

type Config<'S> = 'S * Player

// 'S is for state.
// TODO For railway networks, the state can be a triple of trains pos., signal
// lights, and points.

type OnTheFlySolver<'S when 'S: comparison>(game: Game<'S>, sim: Config<'S> -> Config<'S> -> bool, initState: 'S) =

    // Extract entities from the game definition.
    let edges1, edges2, goals = game

    // Sets for keeping track of winning an losing strategies.
    let win = new HashSet<Vertex>()
    let lose = new HashSet<Vertex>()

    // Dependencies when processesing a given vertex.
    let deps = new List<(Vertex * Vertex list) list>()

    // A set of already-discovered vertices.
    let disc = new HashSet<Vertex>()

    // Connect internal (vertices) and external (configuration) representation.
    // Note to self: It makes sense to use the configs as the vertices because of how the algorithm indexes. 
    let toConfig = new List<Config<'S>>()
    let toVertex = new Dictionary<Config<'S>, Vertex>()

    let vertexOf state =
        match toVertex.TryGetValue(state) with
        | true, vertex -> vertex
        | false, _ ->
            // A newly discovered state with no associated vertex.
            toConfig.Add state
            let vertex = toConfig.Count - 1
            toVertex.Add(state, vertex)

            vertex

    // Transition functions are here simply the edge relations.
    let trans1 state = edges1 state
    let trans2 state = edges2 state

    /// Get a set of possible transitions from a vertex for a given player.
    let trans vertex =
        let state, player = toConfig.[vertex]

        let nextStates =
            match player with
            | One -> trans1 state
            | Two -> trans2 state

        Set.map (fun s -> (s, next player)) nextStates

    let load vertex wait =
        // Only difference between MinSucc and MaxSucc is the following predicate.
        let minMaxSuccPred ((_, i') as c') ((_, i'') as c'') =
            match i', i'' with
            | One, One -> sim c' c''
            | Two, Two -> sim c'' c'
            | _ -> failwith "Simulation relations only legal for same player."

        // Function to collect the successors in MinSucc or MaxSucc into a list.
        let collect (s, i) (ws, ms) ((s', _) as c') =
            if Set.exists (minMaxSuccPred c') ms then
                (ws, ms)
            else
                ((s, i, s') :: ws, Set.add c' ms)

        let succs = trans vertex

        // Return the updated list of waiting edges.
        Set.fold (collect toConfig.[vertex]) (wait, Set.empty) succs
        |> fst








// /// AddSuccessors
// let load v i wait =
//     let (s, _) = toState.[v]
//     let ts = trans (s, i)

//     // MaxSucc or MinSucc respectively dependant on player.
//     let pred (s', _) (s'', _) =
//         match i with
//         | One -> sim (s'', i) (s', i)
//         | Two -> sim (s', i) (s'', i)

//     // Get the successors and load them into the waiting list.
//     // ms is the set of max/min succesors, only needed when adding to wait.
//     Set.fold
//         (fun (ms, ws) (s', _) ->
//             if Set.exists (fun (s'', _) -> pred (s', i) (s'', i)) ms then
//                 (ms, ws)
//             else
//                 (Set.add (s', i) ms, (s, i, s') :: ws))
//         (Set.empty, wait)
//         ts
//     |> snd

// let noWinLose v0 =
//     let (_, i0) as c = toState.[v0]

//     not (Set.forall (fun s' -> sim (s', i0) c && win.[v0] = Some true) states)
//     && not (Set.forall (fun s' -> sim c (s', i0) && lose.[v0] = Some true) states)

// member this.solve initConfig =
//     // Reset data.
//     win.Clear()
//     lose.Clear()
//     deps.Clear()
//     disc.Clear()
//     toNode.Clear()
//     toVertex.Clear()













// let placeholder config = 1

// let succ (transI: List<'S list>) config = transI.[placeholder config]

// let maxSucc sim transI config =
//     List.fold
//         (fun ms c' ->
//             if Set.exists (fun c'' -> sim c'' c') ms then
//                 ms
//             else
//                 Set.add c' ms)
//         Set.empty
//         (succ transI config)

// let minSucc sim transI config =
//     List.fold
//         (fun ms c' ->
//             if Set.exists (sim c') ms then
//                 ms
//             else
//                 Set.add c' ms)
//         Set.empty
//         (succ transI config)

// let load wait sim transI ((s, p) as config) =
//     match p with
//     | One -> maxSucc sim transI config
//     | Two -> maxSucc sim transI config
//     |> Set.fold (fun wait' (s', _) -> (s, p, s') :: wait') wait

// let placeholder2 (c: Config<'S>) : Config<'S> = c

// let solve game sim ((s0, i0) as initConfig) =
//     let (s, a1, a2, n1, n2, g) = game

//     let mutable win, lose = Set.empty, Set.empty

//     let mutable disc = Set.add initConfig Set.empty

//     let mutable deps = Map.add (initConfig, []) Map.empty

//     let wait = load [] sim placeholder2 config

//     let b1 sim wait ((s0, i0) as c) =
//         not (Set.forall (fun s' -> sim (s',i0) c &&  Set.contains (s',i0) win) s)
//         && not (Set.forall (fun s' -> sim c (s',i0) &&  Set.contains (s',i0) lose) s)
//         && not (List.isEmpty wait)

//     let b2 = true

//     let b3 = true

//     let addToWin wait s i =
//         win <- Set.add (s, i) win


//     let rec loop wait' =
//         if b1 sim wait initConfig then ()

//         let (s1, i, s2), tail = List.head wait', List.tail wait'
//         if Set.contains (s1, i) (Set.union win lose) then loop tail

//         if b2 then




//     loop wait initConfig

//     Set.exists (fun s' -> sim (s', i0) (s0, i0)) s


// let next player =
//     match player with
//     | One -> Two
//     | Two -> One
//
// let maxSucc trans sim
//
// let minSuccFold sim ms c' =
//     if Set.exists (sim c') ms then
//         ms
//     else
//         Set.add c' ms
//
// let addSuccessors edges wait (s, i) sim =
//     (wait, edges (s, i)List.fold foldFun wait (edges (s, i))
//     |> List.map ()
//
// let solveGame<'S when 'S : comparison> edges goal sim initConfig =
//
//     let win, lose = new HashSet<Config<'S>>(), new HashSet<Config<'S>>()
//
//     let disc = new HashSet<Config<'S>>()
//     disc.Add(initConfig) |> ignore
//
//     let deps = new Dictionary<Config<'S>, Config<'S> list list>()
//     deps.Add(initConfig, [])
//
//     let wait = addSuccessors edges [] initConfig
//
//     if Set.contains initConfig goal then
//         win.Add(initConfig) |> ignore
//
//     while not (List.isEmpty wait) do
//         let (s1, i, s2), wait = List.head wait, List.tail wait





// TODO Does it make sense to always convert to and from intergers? Could
// the win and dependencies arraylists be dictionaries in stead?
// let toNode = new Dictionary<int, 'N>()
// let toVertex = new Dictionary<'N, int>()
// I am trying doing that below.

// Notes
// - System.Collections.ArrayList is not recommended:
//   https://docs.microsoft.com/en-us/dotnet/api/system.collections.arraylist?view=net-6.0#remarks
// - System.Collections.Generic.List<T> class is used in stead:
//   https://docs.microsoft.com/en-us/dotnet/api/system.collections.generic.list-1?view=net-6.0
// - Documentation recommends System.Collections.Generic.Dictionary over
//   System.Collections.HashTable:
//   https://docs.microsoft.com/en-us/dotnet/api/system.collections.generic.list-1?view=net-6.0
// - Dictionary is implemented as a hash table:
//   https://docs.microsoft.com/en-us/dotnet/api/system.collections.generic.list-1?view=net-6.0
