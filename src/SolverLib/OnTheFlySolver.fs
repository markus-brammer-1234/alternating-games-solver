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

type OnTheFlySolver<'S when 'S: comparison>
    (
        game: Game<'S>,
        simRel: Config<'S> -> Config<'S> -> bool,
        initConfig: Config<'S>
    ) =

    let edgesOne, edgesTwo, goals = game

    // Reversed simulation relation.
    let revSimRel c2 c1 = simRel c1 c2

    // Sets for keeping track of winning an losing strategies.
    let mutable win = Set.empty
    let mutable lose = Set.empty

    // Dependencies when processesing a given vertex.
    // TODO Note the only place where the vertex even matters.
    let deps = new List<('S * Player * 'S) list>()

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
            deps.Add []
            let vertex = toConfig.Count - 1
            toVertex.Add(state, vertex)

            vertex

    let getSuccessorConfigs (state, player) : Set<Config<'S>> =
        // Get the set of successor states from the relevant edges function.
        match player with
        | One ->
            // Player One can also (always) perform the empty action, i.e. stay in current state.
            edgesOne state |> Set.add state
        | Two ->
            let successorStates = edgesTwo state

            // Player Two can only perform empty actions when non other available.
            if Set.isEmpty successorStates then
                set [ state ]
            else
                successorStates

        // In the successor state set, convert the states to configurations.
        |> Set.map (fun state' -> state', next player)

    let load config wait =
        // Only difference between MinSucc and MaxSucc is the following predicate.
        let minMaxSuccPred ((_, i') as c') ((_, i'') as c'') =
            match i', i'' with
            | One, One -> simRel c' c''
            | Two, Two -> simRel c'' c'
            | _ -> false

        let collect (s, i) (ws, ms) =
            function
            | c' when Set.exists (minMaxSuccPred c') ms -> ws, ms
            | (s', _) as c' -> (s, i, s') :: ws, Set.add c' ms

        // Return the updated list of waiting edges.
        getSuccessorConfigs config
        |> Set.fold (collect config) (wait, Set.empty)
        |> fst

    // A configuration have been found that deems the game either won or lost.
    let wonOrLost =
        Set.exists (revSimRel initConfig) win
        || Set.exists (simRel initConfig) lose

    let isLosingConfig (state, player as config) =

        Set.exists (simRel config) lose
        || (player = One
            && getSuccessorConfigs config
               |> Set.forall (fun c' -> Set.exists (simRel c') lose))
        || (player = Two
            && getSuccessorConfigs config
               |> Set.exists (fun c' -> Set.exists (simRel c') lose))
        || (player = Two
            && Set.exists (fun (s', _) -> simRel (s', One) (state, One)) lose) //
        || (Set.isEmpty (edgesOne state)
            && Set.isEmpty (edgesTwo state))

    let isWinningConfig (_, player as config) =

        Set.exists (revSimRel config) win
        || (player = One
            && getSuccessorConfigs config
               |> Set.exists (fun c' -> Set.exists (simRel c') win))
        || (player = Two
            && getSuccessorConfigs config
               |> Set.forall (fun c' -> Set.exists (simRel c') win))

    let inWinLose config =
        Set.contains config win
        || Set.contains config lose

    let rec addDeps (state, player) wait =
        function
        | [] -> wait
        | (s'', p', s') :: ds when
            s' = state
            && p' = next player
            && inWinLose (s'', p')
            ->
            addDeps (state, player) wait ds
        | d :: ds -> addDeps (state, player) (d :: wait) ds

    let addToLose wait c =
        lose <- Set.add c lose
        deps.[vertexOf c] |> addDeps c wait

    let addToWin wait c =
        win <- Set.add c win
        deps.[vertexOf c] |> addDeps c wait

    // Taking an input win' unlike before where I just had finalCheck be a value and used win as the set. This does not work as finalCheck becomes a static boolean value of false.
    let finalCheck win' = Set.exists (revSimRel initConfig) win'

    let rec loop wait =
        match wait with
        | [] -> finalCheck win
        | _ when wonOrLost -> finalCheck win
        | (s1, i, _) :: waitrest when inWinLose (s1, i) -> loop waitrest
        | (s1, i, _) :: waitrest when isLosingConfig (s1, i) -> addToLose waitrest (s1, i) |> loop
        | (s1, i, _) :: waitrest when isWinningConfig (s1, i) -> addToWin waitrest (s1, i) |> loop
        | (_, i, s2) :: waitrest when inWinLose (s2, next i) -> loop waitrest
        | (s1, i, s2) :: waitrest when disc.Contains(vertexOf (s2, next i)) ->
            let v = vertexOf (s2, next i)
            deps.[v] <- (s1, i, s2) :: deps.[v]
            loop waitrest
        | (s1, i, s2) :: waitrest ->
            let config = s2, next i
            let v = vertexOf config
            disc.Add(v) |> ignore
            deps.[v] <- [ (s1, i, s2) ]

            if Set.contains s2 goals then
                addToWin wait config |> loop
            else
                load config waitrest |> loop

    member _.solve =
        disc.Add(vertexOf initConfig) |> ignore
        // if Set.contains s0 goals then addToWin initConfig
        load initConfig [] |> loop

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


// Assuming i1 <> i2 => not (sim (s1, i1) (s2, i2))
