module Nand2Tetris.CPUSimulator.Chips.TruthTables

open Nand2Tetris.CPUSimulator.Types

let Nand a b =
    match (a,b) with
    | (Zero, Zero) -> One
    | (Zero, One)  -> One
    | (One, Zero)  -> One
    | (One, One)   -> Zero

let Not input =
    match input with
    | Zero -> One
    | One  -> Zero

let And a b =
    match (a,b) with
    | (Zero, Zero) -> Zero
    | (Zero, One)  -> Zero
    | (One, Zero)  -> Zero
    | (One, One)   -> One

let Or a b =
    match (a,b) with
    | (Zero, Zero) -> Zero
    | (Zero, One)  -> One
    | (One, Zero)  -> One
    | (One, One)   -> One

let Xor a b =
    match (a,b) with
    | (Zero, Zero) -> Zero
    | (Zero, One)  -> One
    | (One, Zero)  -> One
    | (One, One)   -> Zero

let Mux a b selector =
    match (a,b,selector) with
    | (Zero, Zero, Zero) -> Zero
    | (Zero, One, Zero)  -> Zero
    | (One, Zero, Zero)  -> One
    | (One, One, Zero)   -> One
    | (Zero, Zero, One)  -> Zero
    | (Zero, One, One)   -> One
    | (One, Zero, One)   -> Zero
    | (One, One, One)    -> One

let DMux input selector =
    match selector with
    | Zero -> (input, Zero)
    | One  -> (Zero, input)

let Not16 input =
    Array.map Not input

let And16 a b =
    Array.map2 And a b

let Or16 a b =
    Array.map2 Or a b

let Mux16 a b selector =
    Array.map2 (fun a b -> Mux a b selector) a b

let Or8Way input =
    match input with
    | [| Zero; Zero; Zero; Zero; Zero; Zero; Zero; Zero |] -> Zero
    | _                                                    -> One

let Mux4Way16 (a: Bit array)
              (b: Bit array)
              (c: Bit array)
              (d: Bit array)
              (twoBitSelector: Bit array) =
    match twoBitSelector.[1], twoBitSelector.[0] with
    | Zero, Zero -> a
    | Zero, One  -> b
    | One,  Zero -> c
    | One,  One  -> d

let Mux8Way16 (a: Bit array)
              (b: Bit array)
              (c: Bit array)
              (d: Bit array)
              (e: Bit array)
              (f: Bit array)
              (g: Bit array)
              (h: Bit array)
              (threeBitSelector: Bit array) =
    match threeBitSelector.[2], threeBitSelector.[1], threeBitSelector.[0] with
    | Zero, Zero, Zero -> a
    | Zero, Zero, One  -> b
    | Zero, One,  Zero -> c
    | Zero, One,  One  -> d
    | One,  Zero, Zero -> e
    | One,  Zero, One  -> f
    | One,  One,  Zero -> g
    | One,  One,  One  -> h

let DMux4Way input (twoBitSelector: Bit array) =
    match twoBitSelector.[1], twoBitSelector.[0] with
    | Zero, Zero -> (input, Zero, Zero, Zero)
    | Zero, One  -> (Zero, input, Zero, Zero)
    | One,  Zero -> (Zero, Zero, input, Zero)
    | One,  One  -> (Zero, Zero, Zero, input)

let DMux8Way input (threeBitSelector: Bit array) =
    match threeBitSelector.[2], threeBitSelector.[1], threeBitSelector.[0] with
    | Zero, Zero, Zero -> (input, Zero, Zero, Zero, Zero, Zero, Zero, Zero)
    | Zero, Zero, One  -> (Zero, input, Zero, Zero, Zero, Zero, Zero, Zero)
    | Zero, One,  Zero -> (Zero, Zero, input, Zero, Zero, Zero, Zero, Zero)
    | Zero, One,  One  -> (Zero, Zero, Zero, input, Zero, Zero, Zero, Zero)
    | One,  Zero, Zero -> (Zero, Zero, Zero, Zero, input, Zero, Zero, Zero)
    | One,  Zero, One  -> (Zero, Zero, Zero, Zero, Zero, input, Zero, Zero)
    | One,  One,  Zero -> (Zero, Zero, Zero, Zero, Zero, Zero, input, Zero)
    | One,  One,  One  -> (Zero, Zero, Zero, Zero, Zero, Zero, Zero, input)