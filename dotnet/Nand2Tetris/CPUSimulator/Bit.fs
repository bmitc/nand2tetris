module Nand2Tetris.CPUSimulator.Bit

open System
open System.Collections
open Nand2Tetris.Utilities

type Bit =
    | Zero
    | One

type Adder = { Sum: Bit; Carry: Bit }

type ALUControlBits =
    { zx: Bit
      nx: Bit
      zy: Bit
      ny: Bit
      f:  Bit
      no: Bit }

type ALUOutputBits =
    { zr: Bit
      ng: Bit }

let integerToBits (i: int) length =
    BitArray([|i|])
    |> Seq.cast<bool>
    |> Seq.toArray
    |> Array.map (fun x -> if x then One else Zero)
    |> (fun x -> x.[0..(length-1)])

let integerToBit16 i =
    integerToBits i 16

let allOnes = integerToBit16 0b1111111111111111

let allZeroes = integerToBit16 0b0000000000000000

let bigintToBits (i: bigint) length =
    BitArray(i.ToByteArray())
    |> Seq.cast<bool>
    |> Seq.toArray
    |> resizeArray length
    |> Array.map (fun x -> if x then One else Zero)
    //|> (fun x -> x.[0..(length-1)])

let generatePermutations length =
    let maxInt = (pown 2I length) - 1I
    [for i in 0I..maxInt -> bigintToBits i length]