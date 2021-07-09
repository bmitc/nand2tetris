module Nand2Tetris.CPUSimulator.Bit

open System
open System.Collections
open Nand2Tetris.Utilities

type Bit =
    | Zero
    | One

let integerToBits (i: int) length =
    BitArray([|i|])
    |> Seq.cast<bool>
    |> Seq.toArray
    |> Array.map (fun x -> if x then One else Zero)
    |> (fun x -> x.[0..(length-1)])

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