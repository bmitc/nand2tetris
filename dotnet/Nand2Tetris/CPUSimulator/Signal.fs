module Nand2Tetris.CPUSimulator.Signal

open Nand2Tetris.CPUSimulator.Bit
open Nand2Tetris.CPUSimulator.Chips.Combinatorial

type ClockState =
    | Tick
    | Tock

type Clock = seq<ClockState>

/// Convert a clock state to a bit
let clockToBit clock =
    match clock with
    | Tick -> One
    | Tock -> Zero

//let signalNot (signal: Signal) : Signal = seq {for s in signal -> Not s}

type Signal = seq<Bit>

let private liftToSignal1 op (signal: Signal) : Signal =
    Seq.map op signal

let private liftToSignal2 op (signalA: Signal) (signalB: Signal) : Signal =
    Seq.map2 op signalA signalB

let Id' signal : Signal = signal

let Not' input = liftToSignal1 Not input

let And' a b = liftToSignal2 And a b

let Or' a b = liftToSignal2 Or a b

let Nor' a b = liftToSignal2 Nor a b

let Xor' a b = liftToSignal2 Xor a b

let Mux' a b selector =
    Or' (And' a (Not' selector)) (And' selector b)

let DMux' input selector =
    (And' (Not' selector) input, And' selector input)

/// 16-bit Not gate
let Not16' input =
    Array.map Not' input

/// 16-bit And gate
let And16' a b =
    Seq.map2 And' a b

/// 16-bit Or gate
let Or16' a b =
    Seq.map2 Or' a b

/// 16-bit Mux gate
let Mux16' a b selector =
    Seq.map2 (fun a b -> Mux' a b selector) a b

/// 8-way Or gate
let Or8Way' input =
    Seq.fold Or' (Seq.singleton Zero) input

///// 4-way 16-bit multiplexer
//let Mux4Way16' (a: Bit array)
//               (b: Bit array)
//               (c: Bit array)
//               (d: Bit array)
//               (twoBitSelector: Bit array) =
//    let c0 = Mux16' a c twoBitSelector.[1]
//    let c1 = Mux16' b d twoBitSelector.[1]
//    Mux16' c0 c1 twoBitSelector.[0]

/// 8-way 16-bit multiplexer
let Mux8Way16 (a: Bit array)
              (b: Bit array)
              (c: Bit array)
              (d: Bit array)
              (e: Bit array)
              (f: Bit array)
              (g: Bit array)
              (h: Bit array)
              (threeBitSelector: Bit array) =
    let c0 = Mux16 a e threeBitSelector.[2]
    let c1 = Mux16 b f threeBitSelector.[2]
    let c2 = Mux16 c g threeBitSelector.[2]
    let c3 = Mux16 d h threeBitSelector.[2]
    Mux4Way16 c0 c1 c2 c3 threeBitSelector.[0..1]

/// 4-way demultiplexer
let DMux4Way input (twoBitSelector: Bit array) =
    let c0, c1 = DMux input twoBitSelector.[1]
    let a, b = DMux c0 twoBitSelector.[0]
    let c, d = DMux c1 twoBitSelector.[0]
    (a, b, c, d)

/// 8-way demultiplexer
let DMux8Way input (threeBitSelector: Bit array) =
    let c0, c1 = DMux input threeBitSelector.[2]
    let a, b, c, d = DMux4Way c0 threeBitSelector.[0..1]
    let e, f, g, h = DMux4Way c1 threeBitSelector.[0..1]
    (a, b, c, d, e, f, g, h)

(********************************************
********* Boolean Arithmetic ****************
********************************************)

/// Represents the output of an adder operation
type AdderSignal = { Sum: Signal; Carry: Signal }

/// Half adder
let HalfAdder' a b =
    { Sum = Xor' a b; Carry = And' a b }

/// Full adder
let FullAdder' a b c =
    let { AdderSignal.Sum = c0; Carry = c1 } = HalfAdder' a b
    let { AdderSignal.Sum = sum; Carry = c2 } = HalfAdder' c0 c
    { Sum = sum; Carry = Or' c2 c1 }

///// 16-bit adder
//let Add16 (a: Bit array) (b: Bit array) =
//    let out = Array.init 16 (fun _ -> Zero)
//    let { Sum = out0; Carry = c0 } = HalfAdder a.[0] b.[0]
//    out.[0] <- out0
//    Array.foldi2 (fun index carry x y -> let step = FullAdder carry x y
//                                         out.[index+1] <- step.Sum
//                                         step.Carry)
//                 c0
//                 a.[1..15]
//                 b.[1..15]
//    |> ignore
//    out

/// 16-bit incrementor
let Inc16 (a: Bit array) =
    Add16 a (integerToBit16 1)

/// Preset
let Preset zero negate input =
    Mux4Way16 input (Not16 input) allZeros allOnes [|negate; zero|]

/// 16-way or gate
let Or16Way (input: Bit array) =
    Or (Or8Way input.[0..7]) (Or8Way input.[8..15])

/// 16-bit equal to zero test
let EqZero16 (input: Bit array) =
    Not (Or16Way input)

/// Arithemetic logic unit (ALU)
let ALU controlBits (x: Bit array) (y: Bit array) =
    let c0 = Preset controlBits.zx controlBits.nx x
    let c1 = Preset controlBits.zy controlBits.ny y
    let c2 = And16 c0 c1
    let c3 = Add16 c0 c1
    let c4 = Mux16 c2 c3 controlBits.f
    let c5 = Not16 c4
    let out = Mux16 c4 c5 controlBits.no
    out, {zr = EqZero16 out; ng = out.[15]}

let rec srLatch set reset =
    seq {
        yield Zero
        yield! Nor' reset (Nor' (srLatch set reset) set)
    }

let dLatch data clock =
    srLatch (And' clock data)
            (And' clock (Not' data))

/// Falling or negative edge-triggered D flip-flop (DFF)
let dff (data: Signal) (clock: Clock) : Signal =
    let clockAsSignal = Seq.map clockToBit clock
    dLatch (dLatch data clockAsSignal)
           (Not' clockAsSignal)

//let rec dff (data: Signal) (clock: Clock) : Signal =
//    let clockAsSignal : Signal = Seq.map clockToBit clock
//    seq {
//        yield Zero
//        yield! Or' (And' data clockAsSignal)
//                        (And' (dff data clock) (Not' clockAsSignal))
//    }

let rec positiveEdgeDFF (data: Signal) (clock: Clock) : Signal =
    let clockAsSignal : Signal = Seq.map clockToBit clock
    seq {
        yield Zero
        yield! Or' (And' data clockAsSignal)
                   (And' (positiveEdgeDFF data clock) (Not' clockAsSignal))
    }