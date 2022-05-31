module HornTests

open Horn
open Solver
open System
open Xunit

// See this GitHub repo for tests:
// https://github.com/llefrioux/horn-sat-solver

let isSatisfiable (output: FixedValue array) =
    output.[0] = Zero

[<Fact>]
let ``Simple false Horn formula`` () =
    let actual =
        F [ C [ Pos "p" ]; C [ Neg "p" ] ]
        |> hornToHyperEdgeArray "Special"
        |> Solver.ofArray
        |> Solver.liuSmolka 0

    let expected = [ One; One ]
    Assert.Equal<Collections.Generic.IEnumerable<FixedValue>>(expected, actual)
    Assert.False(isSatisfiable actual)

[<Fact>]
let ``Simple true Horn formula`` () =
    let actual =
        F [ C [ Pos "p" ]; C [ Pos "q" ] ]
        |> hornToHyperEdgeArray "Special"
        |> Solver.ofArray
        |> Solver.liuSmolka 0

    let expected = [ Zero; Unknown; Unknown ]
    Assert.Equal<Collections.Generic.IEnumerable<FixedValue>>(expected, actual)
    Assert.True(isSatisfiable actual)
