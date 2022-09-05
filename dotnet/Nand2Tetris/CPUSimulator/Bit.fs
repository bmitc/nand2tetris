/// Bit data type and associated procesing functions
module Nand2Tetris.CPUSimulator.Bit

open System
open System.Collections
open Nand2Tetris.Utilities

/// Represents a single binary bit
type Bit =
    | Zero
    | One

/// Represents the output of an adder operation
type Adder = { Sum: Bit; Carry: Bit }

/// Represents the ALU control bits
type ALUControlBits =
    { zx: Bit
      nx: Bit
      zy: Bit
      ny: Bit
      f:  Bit
      no: Bit }

/// Represents the ALU output bits
type ALUOutputBits =
    { zr: Bit
      ng: Bit }

/// Converts a boolean to a Bit
let booleanToBit boolean =
    if boolean then One else Zero

/// Converts an integer to a bit array of the given length
let integerToBits (i: int) length =
    BitArray([|i|])
    |> Seq.cast<bool>
    |> Seq.toArray
    |> Array.map booleanToBit
    |> (fun x -> x.[0..(length-1)])

/// Converts an integer to a 16-bit array, i.e., a bit array
/// of length 16
let integerToBit16 i =
    integerToBits i 16

/// A 16-bit array of all ones
let allOnes = integerToBit16 0b1111111111111111

/// A 16-bit array of all zeros
let allZeros = integerToBit16 0b0000000000000000

/// Converts a bigint to a 16-bit array, i.e., a bit array
/// of length 16
let bigintToBits (i: bigint) length =
    BitArray(i.ToByteArray())
    |> Seq.cast<bool>
    |> Seq.toArray
    |> resizeArray length
    |> Array.map (fun x -> if x then One else Zero)
    //|> (fun x -> x.[0..(length-1)])

/// Generates a list of all permutations of a bit array
// for the given length
let generatePermutations length =
    let maxInt = (pown 2I length) - 1I
    [for i in 0I..maxInt -> bigintToBits i length]