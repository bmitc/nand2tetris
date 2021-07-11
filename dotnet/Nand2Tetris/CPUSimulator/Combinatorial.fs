module Nand2Tetris.CPUSimulator.Chips.Combinatorial

open Nand2Tetris.Utilities
open Nand2Tetris.CPUSimulator.Bit


(********************************************
Boolean Logic
********************************************)

let Nand a b =
    match a, b with
    | Zero, Zero -> One
    | Zero, One  -> One
    | One,  Zero -> One
    | One,  One  -> Zero

let Not input =
    Nand input input

let And a b =
    Not (Nand a b)

let Or a b =
    Nand (Not a) (Not b)

let Xor a b =
    And (Nand a b) (Or a b)

let Mux a b selector =
    Or (And a (Not selector)) (And selector b)

let DMux input selector =
    (And (Not selector) input, And selector input)

let Not16 input =
    Array.map Not input

let And16 a b =
    Array.map2 And a b

let Or16 a b =
    Array.map2 Or a b

let Mux16 a b selector =
    Array.map2 (fun a b -> Mux a b selector) a b

let Or8Way input =
    Array.fold Or Zero input

let Mux4Way16 (a: Bit array)
              (b: Bit array)
              (c: Bit array)
              (d: Bit array)
              (twoBitSelector: Bit array) =
    let c0 = Mux16 a c twoBitSelector.[1]
    let c1 = Mux16 b d twoBitSelector.[1]
    Mux16 c0 c1 twoBitSelector.[0]

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

let DMux4Way input (twoBitSelector: Bit array) =
    let c0, c1 = DMux input twoBitSelector.[1]
    let a, b = DMux c0 twoBitSelector.[0]
    let c, d = DMux c1 twoBitSelector.[0]
    (a, b, c, d)

let DMux8Way input (threeBitSelector: Bit array) =
    let c0, c1 = DMux input threeBitSelector.[2]
    let a, b, c, d = DMux4Way c0 threeBitSelector.[0..1]
    let e, f, g, h = DMux4Way c1 threeBitSelector.[0..1]
    (a, b, c, d, e, f, g, h)


(********************************************
Boolean Arithmetic
********************************************)

let HalfAdder a b =
    { Sum = Xor a b; Carry = And a b }

let FullAdder a b c =
    let { Sum = c0; Carry = c1 } = HalfAdder a b
    let { Sum = sum; Carry = c2 } = HalfAdder c0 c
    { Sum = sum; Carry = Or c2 c1 }

let Add16 (a: Bit array) (b: Bit array) =
    let out = Array.init 16 (fun _ -> Zero)
    let { Sum = out0; Carry = c0 } = HalfAdder a.[0] b.[0]
    out.[0] <- out0
    Array.foldi2 (fun index carry x y -> let step = FullAdder carry x y
                                         out.[index+1] <- step.Sum
                                         step.Carry)
                 c0
                 a.[1..15]
                 b.[1..15]
    |> ignore
    out

let Inc16 (a: Bit array) =
    Add16 a (integerToBit16 1)

let Preset zero negate input =
    Mux4Way16 input (Not16 input) allZeroes allOnes [|negate; zero|]

let Or16Way (input: Bit array) =
    Or (Or8Way input.[0..7]) (Or8Way input.[8..15])

let EqZero16 (input: Bit array) =
    Not (Or16Way input)

let ALU controlBits (x: Bit array) (y: Bit array) =
    let c0 = Preset controlBits.zx controlBits.nx x
    let c1 = Preset controlBits.zy controlBits.ny y
    let c2 = And16 c0 c1
    let c3 = Add16 c0 c1
    let c4 = Mux16 c2 c3 controlBits.f
    let c5 = Not16 c4
    let out = Mux16 c4 c5 controlBits.no
    out, {zr = EqZero16 out; ng = out.[15]}