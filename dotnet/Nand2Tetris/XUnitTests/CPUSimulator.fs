module Nand2Tetris.Tests.CPUSimulator

open Xunit
open FsUnit.Xunit
open Nand2Tetris.CPUSimulator.Types
open Nand2Tetris.CPUSimulator.Chips
open Nand2Tetris.CPUSimulator.Chips.Combinatorial

let unaryInput = [Zero; One]
let binaryInput = [(Zero, Zero); (Zero, One); (One, Zero); (One, One)]

let applyUnary (f: Bit -> Bit) = List.map f unaryInput

let applyBinary (f: Bit -> Bit -> Bit) = List.map (fun (a,b) -> f a b) binaryInput

[<Fact>]
let ``Not`` () =
    applyUnary Not |> should equal (applyUnary TruthTables.Not)

[<Fact>]
let ``And`` () =
    applyBinary And |> should equal (applyBinary TruthTables.And)

[<Fact>]
let ``Or`` () =
    applyBinary Or |> should equal (applyBinary TruthTables.Or)

[<Fact>]
let ``Xor`` () =
    applyBinary Xor |> should equal (applyBinary TruthTables.Xor)

[<Fact>]
let ``Mux`` () =
    let input = [ (Zero, Zero, Zero)
                  (Zero, One, Zero)
                  (One, Zero, Zero)
                  (One, One, Zero)
                  (Zero, Zero, One)
                  (Zero, One, One)
                  (One, Zero, One)
                  (One, One, One) ]
    let apply f = List.map (fun (a,b,selector) -> f a b selector) input
    apply Mux |> should equal (apply TruthTables.Mux)

[<Fact>]
let ``DMux`` () =
    let apply (f: Bit -> Bit -> Bit * Bit) = List.map (fun (input, selector) -> f input selector) binaryInput
    apply DMux |> should equal (apply TruthTables.DMux)