module Nand2Tetris.Tests.CPUSimulator

open Xunit
open FsUnit.Xunit
open Nand2Tetris.Utilities
open Nand2Tetris.CPUSimulator.Bit
open Nand2Tetris.CPUSimulator.Chips
open Nand2Tetris.CPUSimulator.Chips.Combinatorial

let unaryInput = [Zero; One]

let binaryInput = [(Zero, Zero); (Zero, One); (One, Zero); (One, One)]

let ternaryInput =
    [ Zero, Zero, Zero 
      Zero, Zero, One
      Zero, One,  Zero
      Zero, One,  One
      One,  Zero, Zero
      One,  Zero, One
      One,  One,  Zero
      One,  One,  One ]

let applyUnary (f: Bit -> Bit) =
    List.map f unaryInput

let applyBinary (f: Bit -> Bit -> 'T) =
    List.map (fun (a, b) -> f a b) binaryInput

let applyTernary (f: Bit -> Bit -> Bit -> 'T) =
    List.map (fun (a, b, c) -> f a b c) ternaryInput

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
    applyTernary Mux |> should equal (applyTernary TruthTables.Mux)

[<Fact>]
let ``DMux`` () =
    let apply (f: Bit -> Bit -> Bit * Bit) = List.map (fun (input, selector) -> f input selector) binaryInput
    apply DMux |> should equal (apply TruthTables.DMux)

[<Fact>]
let ``Or8Way`` () =
    let inputs = generatePermutations 8
    List.map Or8Way inputs |> should equal (List.map TruthTables.Or8Way inputs)

[<Fact>]
let ``Mux4Way16`` () =
    let inputs = generatePermutations 14
                 |> List.map (split [3; 3; 3; 3; 2])
    let applyMux4Way16 implementation =
        let matcher (lst: Bit array list) =
            match lst with
            | [a;b;c;d;sel] -> implementation a b c d sel
            | _             -> failwith "Error with test implementation"
        List.map matcher inputs
    applyMux4Way16 Mux4Way16 |> should equal (applyMux4Way16 TruthTables.Mux4Way16)

[<Fact>]
let ``Mux8Way16`` () =
    let inputs = generatePermutations 19
                 |> List.map (split [2; 2; 2; 2; 2; 2; 2; 2; 3])
    let applyMux8Way16 implementation =
        let matcher (lst: Bit array list) =
            match lst with
            | [a;b;c;d;e;f;g;h;sel] -> implementation a b c d e f g h sel
            | _                     -> failwith "Error with test implementation"
        List.map matcher inputs
    applyMux8Way16 Mux8Way16 |> should equal (applyMux8Way16 TruthTables.Mux8Way16)

[<Fact>]
let ``DMux4Way`` () =
    let inputs = generatePermutations 3
                 |> List.map (split [1; 2])
    let applyDMux4Way implementation =
        let matcher (lst: Bit array list) =
            match lst with
            | [[|input|]; sel] -> implementation input sel
            | _                -> failwith "Error with test implementation"
        List.map matcher inputs
    applyDMux4Way DMux4Way |> should equal (applyDMux4Way TruthTables.DMux4Way)

[<Fact>]
let ``DMux8Way`` () =
    let inputs = generatePermutations 4
                 |> List.map (split [1; 3])
    let applyDMux8Way implementation =
        let matcher (lst: Bit array list) =
            match lst with
            | [[|input|]; sel] -> implementation input sel
            | _                -> failwith "Error with test implementation"
        List.map matcher inputs
    applyDMux8Way DMux8Way |> should equal (applyDMux8Way TruthTables.DMux8Way)

[<Fact>]
let ``HalfAdder`` () =
    applyBinary HalfAdder |> should equal (applyBinary TruthTables.HalfAdder)

[<Fact>]
let ``FullAdder`` () =
    applyTernary FullAdder |> should equal (applyTernary TruthTables.FullAdder)

[<Fact>]
let ``Add16`` () =
    let inputs = List.map (fun (a,b) -> integerToBit16 a, integerToBit16 b)
                          [ 0b0000000000000000, 0b0000000000000000 
                            0b0000000000000000, 0b1111111111111111
                            0b1111111111111111, 0b1111111111111111
                            0b1010101010101010, 0b0101010101010101
                            0b0011110011000011, 0b0000111111110000
                            0b0001001000110100, 0b1001100001110110 ]
    let expected = List.map integerToBit16 [ 0b0000000000000000
                                             0b1111111111111111
                                             0b1111111111111110
                                             0b1111111111111111
                                             0b0100110010110011
                                             0b1010101010101010 ]
    List.map (fun (a,b) -> Add16 a b) inputs |> should equal expected

[<Fact>]
let ``Inc16`` () =
    let inputs = List.map integerToBit16 [ 0b0000000000000000
                                           0b1111111111111111
                                           0b0000000000000101
                                           0b1111111111111011 ]
    let expected = List.map integerToBit16 [ 0b0000000000000001
                                             0b0000000000000000
                                             0b0000000000000110
                                             0b1111111111111100 ]
    List.map Inc16 inputs |> should equal expected