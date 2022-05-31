// Author: Markus Brammer Jensen, s183816 
// Based on Horn description in [liu1998a] (see Bibliography.txt). 

module Horn

type Literal =
    | Pos of string
    | Neg of string

type Clause = C of Literal list

type Formula = F of Clause list

let isPosLit =
    function
    | Pos _ -> true
    | Neg _ -> false

let tryFindPosLit (C clause) =
    match List.filter isPosLit clause with
    | [] -> None
    | [ Pos p ] -> Some p
    | _ -> failwith "Not a Horn clause: Too many positive literals."

let isNegLit literal = not (isPosLit literal)

let getNegLits (C clause) = List.filter isNegLit clause

let getLetter = 
    function 
    | Pos p 
    | Neg p -> p

// Try with List.collect
let getDict special (F formula) = 
    formula 
    |> List.map (fun (C c) -> c)
    |> List.concat 
    |> List.map getLetter 
    |> List.distinct
    |> fun distinctPropLetters -> special :: distinctPropLetters
    |> fun list -> List.zip list [ 0 .. List.length list - 1 ]
    |> Map.ofList

let letterToNum dict a = Map.find a dict

let hornToHyperEdgeArray v0 (F formula) =
    let dict = getDict v0 (F formula)
    let edges = Array.init (Map.count dict) (fun _ -> [])
    for C literals as clause in formula do 
        match tryFindPosLit clause with 
        | None -> 
            let index = Map.find v0 dict
            let newEdge = List.map (getLetter >> letterToNum dict) literals
            edges.[index] <- newEdge :: edges.[index]
        | Some p -> 
            let index = Map.find p dict
            let newEdge = 
                getNegLits clause 
                |> List.map (getLetter >> letterToNum dict)
            edges.[index] <- newEdge :: edges.[index]
    edges
    
