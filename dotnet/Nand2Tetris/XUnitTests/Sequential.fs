module Nand2Tetris.Tests.CPUSimulator.Sequential

open Xunit
open FsUnit.Xunit
open Nand2Tetris.Utilities
open Nand2Tetris.CPUSimulator.Bit
open Nand2Tetris.CPUSimulator.Chips.Sequential

/// Given a list of inputs, generates clock cycles and returns a list of
/// tuples consisting of the inputs as the first element of the return
/// tuple and the clock as the second element.
let generateInputs inputs =
    let numberOfCycles = List.length inputs
    let clockCycles = generateClockCycles numberOfCycles
    let duplicateInputs = interleave inputs inputs
    List.zip duplicateInputs clockCycles

[<Fact>]
let ``DFF test`` () =
    let inputs = generateInputs [One; Zero; Zero; One; Zero]
    let expected = duplicateElements [Zero; One; Zero; Zero; One]
    let dff = DFF()
    List.map (fun (input, clock) -> dff.Compute input clock) inputs
    |> should equal expected

[<Fact>]
let ``DFF with Zero values on Tick`` () =
    let initialInputs = generateInputs [One; Zero; Zero; One; Zero]
    let inputs = List.map (fun (input, clock) -> match clock with
                                                 | Tick -> Zero, clock
                                                 | Tock -> input, clock)
                          initialInputs
    let expected = duplicateElements [Zero; One; Zero; Zero; One]
    let dff = DFF()
    List.map (fun (input, clock) -> dff.Compute input clock) inputs
    |> should equal expected

[<Fact>]
let ``DFF with One values on Tick`` () =
    let initialInputs = generateInputs [One; Zero; Zero; One; Zero]
    let inputs = List.map (fun (input, clock) -> match clock with
                                                 | Tick -> One, clock
                                                 | Tock -> input, clock)
                          initialInputs
    let expected = duplicateElements [Zero; One; Zero; Zero; One]
    let dff = DFF()
    List.map (fun (input, clock) -> dff.Compute input clock) inputs
    |> should equal expected

[<Fact>]
let ``BitRegister test`` () =
    let inputs = generateInputs [One,One; Zero,Zero; Zero,Zero; Zero,One; Zero,Zero]
    let expected = duplicateElements [Zero; One; One; One; Zero]
    let bit = BitRegister()
    List.map (fun ((input, load), clock) -> bit.Compute input load clock) inputs
    |> should equal expected

[<Fact>]
let ``Register test`` () =
    let inputs = [0,Zero; 12345,Zero; 12345,One; -12345,One; 12345,Zero; 0,Zero]
                 |> List.map (fun (input, load) -> integerToBit16 input, load)
                 |> generateInputs
    let expected = [0; 0; 0; 12345; -12345; -12345]
                   |> List.map integerToBit16
                   |> duplicateElements
    let register = Register()
    List.map (fun ((input, load), clock) -> register.Compute input load clock) inputs
    |> should equal expected

[<Fact>]
let ``PC test`` () =
    //             input   reset load  inc
    let inputs = [ 0,      Zero, Zero, Zero
                   0,      Zero, Zero, One
                   -32123, Zero, Zero, One
                   -32123, Zero, One,  One
                   -32123, Zero, Zero, One
                   -32123, Zero, Zero, One
                   12345,  Zero, One,  Zero
                   12345,  One,  One,  Zero
                   12345,  Zero, One,  One
                   12345,  One,  One,  One
                   12345,  Zero, Zero, One
                   12345,  One,  Zero, One
                   0,      Zero, One,  One
                   0,      Zero, Zero, One
                   22222,  One,  Zero, Zero
                   22222,  Zero, Zero, Zero ]
                 |> List.map (fun (input, reset, load, inc) -> integerToBit16 input, reset, load, inc)
                 |> generateInputs
    let expected = [ 0; 0; 1; 2; -32123; -32122; -32121; 12345; 0; 12345; 0; 1; 0; 0; 1; 0]
                   |> List.map integerToBit16
                   |> duplicateElements
    let pc = PC()
    List.map (fun ((input, reset, load, increment), clock) -> pc.Compute input load increment reset clock) inputs
    |> should equal expected