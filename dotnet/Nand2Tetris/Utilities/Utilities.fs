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