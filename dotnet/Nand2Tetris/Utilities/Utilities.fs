module Nand2Tetris.Utilities

open System.Text.RegularExpressions

/// Takes a list of strings and returns an option regular expression string.
/// For example, ["1"; "2"; "3"] is turned into "1|2|3".
let createOptionRegex lst =
    lst
    |> List.map Regex.Escape
    |> List.reduce (fun x y -> x + "|" + y)

/// Generic partial active pattern that will match a regular expression and
/// then destructure into a list of the match's groups, excluding the whole group.
let (|RegexMatch|_|) pattern (input: string) =
    let m = Regex.Match (input.Trim(), pattern, RegexOptions.Compiled)
    if m.Success
    then Some (List.tail [for group in m.Groups -> group.Value.Trim()] )
    else None

/// Read a file into a list of strings, where each line in the file is a new element in the list.
let readLines filePath = System.IO.File.ReadLines filePath |> Seq.toList

/// Splits an array into a list of separate arrays for each length in lengths
/// For example, lengths of [2;3] splits the array [|1;2;3;4;5|] into the list
/// of arrays [ [|1;2|]; [|3;4;5|] ].
let rec split lengths arr =
    match lengths with
    | []        -> []
    | i :: tail -> let (a,b) = Array.splitAt i arr
                   a :: (split tail b)

/// Resizes an array to the given length. If the length is greater than the array's length,
/// the array is padded by Unchecked.defaultof<'T>. If the length is less than the array's
/// length, then the extra elements are removed. If the length's are the same, the array is
/// returned unchanged.
let resizeArray length (arr: 'T array) =
    match arr.Length with
    | l when l < length -> let padding = Array.zeroCreate<'T> (length-l)
                           Array.concat [arr; padding]
    | l when l > length -> arr.[0..(length-1)]
    | _                 -> arr

/// Interleaves the elements from the two lists.
/// For example, interleave [0;2;4] [1;3;5] will return [0;1;2;3;4;5].
let interleave list1 list2 =
    List.foldBack2 (fun a b xs -> a :: b :: xs) list1 list2 []

/// Checks if list1 is a subset of or equal to list2
let isSublist list1 list2 =
    List.forall (fun x -> List.contains x list2) list1

/// Tests if the two lists are equal when treated as sets, i.e.,
/// the order of the elements does not matter in the equality test
let listEqualityAsSets list1 list2 =
    (isSublist list1 list2) && (isSublist list1 list2)

/// Returns a list where each element has been duplicated.
/// For example, duplicateElements [1;2;3] returns [1;1;2;2;3;3].
let duplicateElements list =
    interleave list list

module Array =
    /// This is the same as Array.fold2 except the index of each element is given to the folder function
    let foldi2 (folder: int -> 'State -> 'T1 -> 'T2 -> 'State) state array1 array2 =
        Array.fold2 (fun (i, s) a b -> i+1, folder i s a b)
                    (0, state)
                    array1
                    array2
        |> snd