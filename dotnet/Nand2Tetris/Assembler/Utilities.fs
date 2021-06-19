module Assembler.Utilities

open System

/// Convert an integer to a string of bits representing the binary
/// representation of the integer padded with zeros to make the
/// string numberOfBits wide.
let binaryToString (i: int) numberOfBits =
    Convert.ToString(int i,2).PadLeft(numberOfBits, '0')

/// Maps a function across both the key and value. Note that Map.map only maps along the value and not the key.
let mapKeyAndValue (f: 'Key * 'T -> 'NewKey * 'U) (map: Map<'Key,'T>) : Map<'NewKey,'U> =
    map
    |> Map.toList
    |> List.map f
    |> Map.ofList