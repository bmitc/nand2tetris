module Nand2Tetris.CPUSimulator.Chips.Sequential

open Nand2Tetris.CPUSimulator.Bit
open Nand2Tetris.CPUSimulator.Chips.Combinatorial

type Clock =
    | Tick
    | Tock

let generateClockCycles numberOfCycles =
    let even x = (x % 2) = 0
    List.init (2*numberOfCycles) (fun i -> if (even i) then Tick else Tock)

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
        | true  -> output <- previousInput
        previousInput <- input
        previousClock <- clock
        output

type BitRegister() =
    let dff = DFF()

    member _.Output = dff.Output

    member _.Compute input load clock =
        let c0 = Mux dff.Output input load
        let output = dff.Compute c0 clock
        output

type Register() =
    let bits = Array.init 16 (fun _ -> BitRegister())

    member _.Output =
        Array.map (fun (x: BitRegister) -> x.Output) bits

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

type PC() =
    let register = Register()
    
    member _.Compute input load increment reset clock =
        let c0 = Or reset load
        let c1 = Or c0 increment
        let c3 = Inc16 register.Output
        let c4 = Mux8Way16 input
                           c3
                           input
                           input
                           allZeroes
                           allZeroes
                           allZeroes
                           allZeroes
                           [|increment; load; reset|]
        let c2 = register.Compute c4 c1 clock
        c2

type RAM512() =
    let RAM64s = Array.init 8 (fun _ -> RAM64())

    member _.Compute input load (address: Bit array) clock =
        let (l0,l1,l2,l3,l4,l5,l6,l7) = DMux8Way load address.[6..8]
        let reg = Array.mapi2 (fun i (ram64: RAM64) load -> ram64.Compute input load address.[0..5] clock)
                              RAM64s
                              [|l0;l1;l2;l3;l4;l5;l6;l7|]
        Mux8Way16 reg.[0] reg.[1] reg.[2] reg.[3] reg.[4] reg.[5] reg.[6] reg.[7] address.[6..8]

type RAM4K() =
    let RAM512s = Array.init 8 (fun _ -> RAM512())

    member _.Compute input load (address: Bit array) clock =
        let (l0,l1,l2,l3,l4,l5,l6,l7) = DMux8Way load address.[9..11]
        let reg = Array.mapi2 (fun i (ram512: RAM512) load -> ram512.Compute input load address.[0..8] clock)
                              RAM512s
                              [|l0;l1;l2;l3;l4;l5;l6;l7|]
        Mux8Way16 reg.[0] reg.[1] reg.[2] reg.[3] reg.[4] reg.[5] reg.[6] reg.[7] address.[9..11]

type RAM16K() =
    let RAM4Ks = Array.init 8 (fun _ -> RAM4K())

    member _.Compute input load (address: Bit array) clock =
        let (l0,l1,l2,l3) = DMux4Way load address.[12..13]
        let reg = Array.mapi2 (fun i (ram4k: RAM4K) load -> ram4k.Compute input load address.[0..11] clock)
                              RAM4Ks
                              [|l0;l1;l2;l3|]
        Mux4Way16 reg.[0] reg.[1] reg.[2] reg.[3] address.[12..13]