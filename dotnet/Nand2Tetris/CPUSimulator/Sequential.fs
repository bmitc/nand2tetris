module Nand2Tetris.CPUSimulator.Chips.Sequential

open Nand2Tetris.CPUSimulator.Bit
open Nand2Tetris.CPUSimulator.Chips.Combinatorial

type Clock =
    | Tick
    | Tock

let newCycle previousClock newClock =
    match previousClock, newClock with
    | Tock, Tick -> true
    | _          -> false


type IClockable =
    abstract member Clock: Clock -> unit

type DFF() =
    let mutable previousClock = Tick
    let mutable previousInput = Zero
    let mutable output = Zero

    member _.Output = output

    member _.Compute input clock =
        match newCycle previousClock clock with
        | false -> ()
        | true -> output <- previousInput
        previousInput <- input
        previousClock <- clock
        output

type BitRegister() =
    let dff = DFF()

    member _.Compute input load clock =
        let c0 = Mux dff.Output input load
        let output = dff.Compute c0 clock
        output

type Register() =
    let bits = Array.init 16 (fun _ -> BitRegister())

    member _.Compute (input: Bit array) load clock =
        Array.mapi (fun i (x: BitRegister) -> x.Compute input.[i] load clock) bits

type RAM8() =
    let registers = Array.init 8 (fun _ -> Register())

    member _.Compute (input: Bit array) load (address: Bit array) clock =
        let (l0,l1,l2,l3,l4,l5,l6,l7) = DMux8Way load address
        let reg = Array.mapi2 (fun i (register: Register) load -> register.Compute input load clock)
                              registers
                              [|l0;l1;l2;l3;l4;l5;l6;l7|]
        Mux8Way16 reg.[0] reg.[1] reg.[2] reg.[3] reg.[4] reg.[5] reg.[6] reg.[7] address

type RAM64() =
    let RAM8s = Array.init 8 (fun _ -> RAM8())

    member _.Compute (input: Bit array) load (address: Bit array) clock =
        let (l0,l1,l2,l3,l4,l5,l6,l7) = DMux8Way load address.[3..5]
        let reg = Array.mapi2 (fun i (ram8: RAM8) load -> ram8.Compute input load address.[0..2] clock)
                              RAM8s
                              [|l0;l1;l2;l3;l4;l5;l6;l7|]
        Mux8Way16 reg.[0] reg.[1] reg.[2] reg.[3] reg.[4] reg.[5] reg.[6] reg.[7] address