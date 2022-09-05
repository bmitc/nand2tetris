/// Truth table definitions for chips, to be used primarily for testing
module Nand2Tetris.CPUSimulator.Chips.TruthTables

open Nand2Tetris.CPUSimulator.Bit

/// Nand truth table
let Nand a b =
    match a, b with
    | Zero, Zero -> One
    | Zero, One  -> One
    | One,  Zero -> One
    | One,  One  -> Zero

/// Not truth table
let Not input =
    match input with
    | Zero -> One
    | One  -> Zero

/// And truth table
let And a b =
    match a, b with
    | Zero, Zero -> Zero
    | Zero, One  -> Zero
    | One,  Zero -> Zero
    | One,  One  -> One

/// Or truth table
let Or a b =
    match a, b with
    | Zero, Zero -> Zero
    | Zero, One  -> One
    | One,  Zero -> One
    | One,  One  -> One

/// Xor truth table
let Xor a b =
    match a, b with
    | Zero, Zero -> Zero
    | Zero, One  -> One
    | One,  Zero -> One
    | One,  One  -> Zero

/// Mux truth table
let Mux a b selector =
    match a, b, selector with
    | Zero, Zero, Zero -> Zero
    | Zero, One,  Zero -> Zero
    | One,  Zero, Zero -> One
    | One,  One,  Zero -> One
    | Zero, Zero, One  -> Zero
    | Zero, One,  One  -> One
    | One,  Zero, One  -> Zero
    | One,  One,  One  -> One

/// DMux truth table
let DMux input selector =
    match selector with
    | Zero -> input, Zero
    | One  -> Zero, input

/// 16-bit Not truth table
let Not16 input =
    Array.map Not input

/// 16-bit And truth table
let And16 a b =
    Array.map2 And a b

/// 16-bit Or truth table
let Or16 a b =
    Array.map2 Or a b

/// 16-bit Mux truth table
let Mux16 a b selector =
    Array.map2 (fun a b -> Mux a b selector) a b

/// 8-way Or truth table
let Or8Way input =
    match input with
    | [| Zero; Zero; Zero; Zero; Zero; Zero; Zero; Zero |] -> Zero
    | _                                                    -> One

/// 4-way 16-bit Mux truth table
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

/// 8-way 16-bit Mux truth table
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

/// 4-way DMux truth table
let DMux4Way input (twoBitSelector: Bit array) =
    match twoBitSelector.[1], twoBitSelector.[0] with
    | Zero, Zero -> input, Zero, Zero, Zero
    | Zero, One  -> Zero, input, Zero, Zero
    | One,  Zero -> Zero, Zero, input, Zero
    | One,  One  -> Zero, Zero, Zero, input

/// 8-way DMux truth table
let DMux8Way input (threeBitSelector: Bit array) =
    match threeBitSelector.[2], threeBitSelector.[1], threeBitSelector.[0] with
    | Zero, Zero, Zero -> input, Zero, Zero, Zero, Zero, Zero, Zero, Zero
    | Zero, Zero, One  -> Zero, input, Zero, Zero, Zero, Zero, Zero, Zero
    | Zero, One,  Zero -> Zero, Zero, input, Zero, Zero, Zero, Zero, Zero
    | Zero, One,  One  -> Zero, Zero, Zero, input, Zero, Zero, Zero, Zero
    | One,  Zero, Zero -> Zero, Zero, Zero, Zero, input, Zero, Zero, Zero
    | One,  Zero, One  -> Zero, Zero, Zero, Zero, Zero, input, Zero, Zero
    | One,  One,  Zero -> Zero, Zero, Zero, Zero, Zero, Zero, input, Zero
    | One,  One,  One  -> Zero, Zero, Zero, Zero, Zero, Zero, Zero, input

/// Half adder truth table
let HalfAdder a b =
    match a, b with
    | Zero, Zero -> { Sum = Zero; Carry = Zero }
    | Zero, One  -> { Sum = One;  Carry = Zero }
    | One,  Zero -> { Sum = One;  Carry = Zero }
    | One,  One  -> { Sum = Zero; Carry = One  }

/// Full adder truth table
let FullAdder a b c =
    match a, b, c with
    | Zero, Zero, Zero -> { Sum = Zero; Carry = Zero }
    | Zero, Zero, One  -> { Sum = One;  Carry = Zero }
    | Zero, One,  Zero -> { Sum = One;  Carry = Zero }
    | Zero, One,  One  -> { Sum = Zero; Carry = One  }
    | One,  Zero, Zero -> { Sum = One;  Carry = Zero }
    | One,  Zero, One  -> { Sum = Zero; Carry = One  }
    | One,  One,  Zero -> { Sum = Zero; Carry = One  }
    | One,  One,  One  -> { Sum = One;  Carry = One  }

/// Preset truth table
let Preset zero negate input =
    match zero, negate with
    | Zero, Zero -> input
    | Zero, One  -> Not16 input
    | One,  Zero -> allZeros
    | One,  One  -> allOnes

//let ALU (x: Bit array) (y: Bit array) controlBits =
//    let negateNumber (input: Bit array) =
//        let negationBit =
//            match input.[15] with
//            | Zero -> One
//            | One  -> Zero
//        Array.concat [input.[0..14]; [|negationBit|]]
//    let output =
//        match controlBits with
//        | {zx=One;  nx=Zero; zy=One;  ny=Zero; f=One;  no=Zero} -> integerToBit16 0
//        | {zx=One;  nx=One;  zy=One;  ny=One;  f=One;  no=One}  -> integerToBit16 1
//        | {zx=One;  nx=One;  zy=One;  ny=Zero; f=One;  no=Zero} -> integerToBit16 -1
//        | {zx=Zero; nx=Zero; zy=One;  ny=One;  f=Zero; no=Zero} -> x
//        | {zx=One;  nx=One;  zy=Zero; ny=Zero; f=Zero; no=Zero} -> y
//        | {zx=Zero; nx=Zero; zy=One;  ny=One;  f=Zero; no=One}  -> Not16 x
//        | {zx=One;  nx=One;  zy=Zero; ny=Zero; f=Zero; no=One}  -> Not16 y
//        | {zx=Zero; ny=Zero; zy=One;  ny=One;  f=One;  no=One}  -> negateNumber x
//        | {zx=One;  ny=One;  zy=Zero; ny=Zero; f=One;  no=One}  -> negateNumber y
//        | {zx=Zero; nx=One;  zy=One;  ny=One;  f=One;  no=One}  -> 
//        output