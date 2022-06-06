// Author: Markus Brammer Jensen, s183816
// Note:   The concepts are explained in depth in bachelor thesis. To make it a
//         bit more clear: up-direction = right, down-direction = left.

namespace RailwayLib

type Port =
    | L of string // Linear segment.
    | S of string // Stem of point.
    | P of string // Plus (default) branch of point.
    | M of string // Minus branch of point.

type Network =
    | N of
        Map<Port, Port> *  // Connections (down to up).
        Set<Port> *  // Set of ALL signal placements.
        List<Port * Port> // Trains' initial and final locations.


module ParserFunctions =

    let toPort id =
        function
        | "up"
        | "down" -> L id
        | "stem" -> S id
        | "plus" -> P id
        | "minus" -> M id
        | s -> failwith $"\"%s{s}\" is not a recognized port kind."

    let toSignal id =
        function
        | "up" -> (id, Up)
        | "down" -> (id, Down)
        | s -> failwith $"\"%s{s}\" is not a recognized signal direction."


/// Library of functions checking if a network is well-formed.
module NetworkFunctions =

    let isLinear = 
        function 
        | L _ -> true 
        | _ -> false 

    let getPortId =
        function
        | L id
        | S id
        | P id
        | M id -> id

    let extractConnection (linears, points) port1 port2 =
        match port1, port2 with
        | L id1, L id2 -> (Set.add id1 linears |> Set.add id2, points)
        | L id1, p2 ->
            let id2 = getPortId p2
            (Set.add id1 linears, Set.add id2 points)
        | p1, L id2 ->
            let id1 = getPortId p1
            (Set.add id2 linears, Set.add id1 points)
        | p1, p2 ->
            let id1, id2 = getPortId p1, getPortId p2
            (linears, Set.add id1 points |> Set.add id2)

    // ! Can be merged with extractConnection.
    let connectionInvariant port1 port2 =
        match port1, port2 with
        | L _, L _ -> false
        | L _, _ -> true
        | _, L _ -> true
        | _ -> false

    let pointInvariant fromPorts toPorts id = 
        match List.tryFind (fun port -> getPortId port = id) fromPorts with 
        | Some (S id') -> 
            List.contains (P id') toPorts 
            && List.contains (M id') toPorts
            && not (List.contains (S id') toPorts)
        | Some (M id') -> 
            List.contains (P id') fromPorts 
            && List.contains (S id') toPorts
            && not (List.contains (M id') toPorts)
        | Some (P id') -> 
            List.contains (M id') fromPorts 
            && List.contains (S id') toPorts
            && not (List.contains (P id') toPorts)
        | _ -> false 

    let isWellFormed (N (cs, ss, ts)) =
        let linears, points = 
            Map.fold extractConnection (Set.empty, Set.empty) cs

        let fromPorts, toPorts = 
            Map.keys cs |> Seq.toList, Map.values cs |> Seq.toList
        
        let trainFinalPos = Map.values ts |> Seq.toList 

        // A port can at most appear once as an origin and once as a
        // destination. 
        fromPorts = List.distinct fromPorts && toPorts = List.distinct toPorts

        // Point invariant. 
        && Set.forall (pointInvariant fromPorts toPorts) points

        // Only connect a linear segment and a port.
        && Map.forall connectionInvariant cs

        // No linear segment and point have common ID.
        && Set.forall (fun id -> Set.contains id points |> not) linears

        // Signal placed on an existing linear segment.
        && Map.forall (fun id _ -> Set.exists ((=) id) linears) ss

        // Trains' initial and final location placed on an existing linear
        // segment. 
        && Map.forall
            (fun init final -> 
                Set.contains init linears && Set.contains final linears)
            ts

        // Trains cannot have the same final position.
        && trainFinalPos = List.distinct trainFinalPos

        // No cycles.
        // TODO Implement. 
        && true

module GameFunctions = 

    open OnTheFlySolver

    type TrainGameState = 
        Port array *    // Positions of trains travelling UP
        Port array *    // Positions of trains tavelling DOWN 
        Set<Port> *     // Positions of signals that allow passage. 
        Set<string>     // Points in MINUS.  


    // type TrainGameState = 
    //     | T of 
    

    // let toGame (N (cs, ss, ts)) = 

    //     let initState 




