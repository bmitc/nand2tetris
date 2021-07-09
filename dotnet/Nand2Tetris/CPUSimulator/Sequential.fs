module Nand2Tetris.CPUSimulator.Chips.Combinatorial

open Nand2Tetris.CPUSimulator.Bit

let Nand a b =
    match (a,b) with
    | (Zero, Zero) -> One
    | (Zero, One)  -> One
    | (One,  Zero) -> One
    | (One,  One)  -> Zero

let Not input =
    Nand input input

let And a b =
    Not(Nand a b)

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
    let (c0, c1) = DMux input twoBitSelector.[1]
    let (a, b) = DMux c0 twoBitSelector.[0]
    let (c, d) = DMux c1 twoBitSelector.[0]
    (a,b,c,d)

let DMux8Way input (threeBitSelector: Bit array) =
    let (c0, c1) = DMux input threeBitSelector.[2]
    let (a,b,c,d) = DMux4Way c0 threeBitSelector.[0..1]
    let (e,f,g,h) = DMux4Way c1 threeBitSelector.[0..1]
    (a,b,c,d,e,f,g,h)