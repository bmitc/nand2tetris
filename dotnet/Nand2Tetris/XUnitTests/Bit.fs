module Nand2Tetris.Tests.CPUSimulator.Bit

open Xunit
open FsUnit.Xunit
open Nand2Tetris.Utilities
open Nand2Tetris.CPUSimulator.Bit

[<Fact>]
let ``3 as 3-bit array`` () =
    integerToBits 3 3 |> should equal [|One; One; Zero|]

[<Fact>]
let ``7 as 3-bit array`` () =
    integerToBits 7 3 |> should equal [|One; One; One|]

[<Fact>]
let ``5 as 5-bit array`` () =
    integerToBits 5 5 |> should equal [|One; Zero; One; Zero; Zero|]

[<Fact>]
let ``7 as 8-bit array`` () =
    integerToBits 7 8 |> should equal [|One; One; One; Zero; Zero; Zero; Zero; Zero|]

[<Fact>]
let ``All zeros`` () =
    allZeros |> should equal (Array.create 16 Zero)

[<Fact>]
let ``All ones``() =
    allOnes |> should equal (Array.create 16 One)

[<Fact>]
let ``Permutations of length 2`` () =
    let permutations = generatePermutations 2
    let expected = [
        [| Zero; Zero |];
        [| Zero; One  |];
        [| One;  Zero |];
        [| One;  One  |]
    ]
    listEqualityAsSets permutations expected
    |> should be True

[<Fact>]
let ``Permutations of length 3`` () =
    let permutations = generatePermutations 3
    let expected = [
        [| Zero; Zero; Zero |];
        [| Zero; Zero; One  |];
        [| Zero; One;  Zero |];
        [| Zero; One;  One  |];
        [| One;  Zero; Zero |];
        [| One;  Zero; One  |];
        [| One;  One;  Zero |];
        [| One;  One;  One  |]
    ]
    listEqualityAsSets permutations expected
    |> should be True