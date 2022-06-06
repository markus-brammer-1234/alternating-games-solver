module OnTheFlySolverTests 

open OnTheFlySolver
open Xunit


// |----| |-
//   L1     \
//           \ 
// |----| |----| |----| 
//   L2     P1     L3
//
// A tuple of train position and point connection (+ or -). 
// Train can only drive in one direction.
let simpleGame = 
    let edges1 (pos, _) = set [ (pos, "+"); (pos, "-") ]

    let edges2 = 
        function 
        | ("L3", p) when p = "+" -> set [ ("L2", p) ]
        | ("L3", p) when p = "-" -> set [ ("Crash", p) ]
        | _ -> set []

    let goal = set [ ("L2", "+"); ("L2", "-") ]

    (edges1, edges2, goal)
    
[<Fact>]
let ``Simple on the fly solver`` () = 
    let solver = new OnTheFlySolver<string * string>(simpleGame, (=), (("L3", "-"), One))
    Assert.True(solver.solve) 
    
[<Fact>]
let ``Simple on the fly solver 2`` () = 
    let solver = new OnTheFlySolver<string * string>(simpleGame, (=), (("L1", "+"), One))
    Assert.False(solver.solve) 