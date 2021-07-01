module VirtualMachine.Core

open Nand2Tetris.Utilities

type Data = Data of int16

type Segment =
    | Argument
    | Local
    | Static
    | Constant
    | This
    | That
    | Pointer
    | Temp

let convertSegmentToVariable = function
    | Argument -> "ARG"
    | Local    -> "LCL"
    | This     -> "THIS"
    | That     -> "THAT"
    | _        -> ""

let incrementSP =
    ["// Increment SP";
     "@SP";
     "M=M+1"]

let decrementSPAndPopStackToD =
    ["// Decrement SP and pop stack to D"
     "@SP"
     "AM=M-1"
     "D=M"]

let pushDToStack =
    ["// Push D to stack";
     "@SP";
     "A=M";
     "M=D"]

let popFromStackToD =
    ["// Pop from stack to D";
     "@SP";
     "A=M";
     "D=M"]

let push segment (index: uint16) staticPrefix =
    let segmentString = convertSegmentToVariable segment
    match segment, index with
    | Pointer, 0us       -> ["@THIS"
                             "D=M"]
                            @pushDToStack @ incrementSP
    | Pointer, 1us       -> ["@THAT"
                             "D=M"]
                            @pushDToStack @ incrementSP
    | Constant, 0us 
    | Constant, 1us      -> [$"// Push {index} directly to stack"
                             "@SP";
                             "A=M";
                             $"M={index}"]
                            @ incrementSP
    | Constant, constant -> ["// Load constant to D"
                             $"@{constant}"
                             "D=A"]
                            @ pushDToStack @ incrementSP
    | Static, _          -> ["// Load staticPrefix.i variable to D"
                             $"@{staticPrefix}.{index}"
                             "D=M"]
                            @pushDToStack @ incrementSP
    | Temp, _            -> ["// Load RAM[5+i] to D"
                             $"@{5us+index}"
                             "D=M"]
                            @ pushDToStack @ incrementSP
    | _, 0us             -> ["// Load segment[0] to D"
                             $"@{segmentString}"
                             "A=M"
                             "D=M"]
                            @ pushDToStack @ incrementSP
    | _, 1us             -> ["// Load segment[1] to D"
                             $"@{segmentString}"
                             "A=M+1"
                             "D=M"]
                            @ pushDToStack @ incrementSP
    | _, _               -> ["// Load segment[index] to D"
                             $"@{index}"
                             "D=A"
                             $"@{segmentString}"
                             "A=D+M"
                             "D=M"]
                            @ pushDToStack @ incrementSP

let pop segment (index: uint16) staticPrefix =
    let segmentString = convertSegmentToVariable segment
    match segment, index with
    | Pointer, 0us       -> decrementSPAndPopStackToD @
                            ["@THIS"
                             "M=D"]
    | Pointer, 1us       -> decrementSPAndPopStackToD @
                            ["@THAT"
                             "M=D"]
    | Constant, _        -> []
    | Static, _          -> decrementSPAndPopStackToD @
                            ["// Load D to staticPrefix.i variable"
                             $"@{staticPrefix}.{index}"
                             "M=D"]
    | Temp, _            -> decrementSPAndPopStackToD @
                            [$"@{5us+index}"
                             "M=D"]
    | _, _               -> ["// Load segment[index] to D"
                             $"@{index}"
                             "D=A"
                             $"@{segmentString}"
                             "D=D+M"
                             "// Store segment[index] address in R13"
                             "@R13"
                             "M=D"]
                            @ decrementSPAndPopStackToD @
                            ["@R13"
                             "A=M"
                             "M=D"]

type ArithmeticLogicCommand =
    | Add
    | Subtract
    | Negate
    | Equal
    | GreaterThan
    | LessThan
    | And
    | Or
    | Not

let translateArithmeticLogicCommand command =
    match command with
    | Add -> ["// Pop stack to D"
              "@SP"
              "AM=M-1"
              "D=M"
              "// "
              "A=A-1"
              "// "
              "M=D+M"]
    | Subtract -> ["// Pop stack to D"
                   "@SP"
                   "AM=M-1"
                   "D=M"
                   "// "
                   "A=A-1"
                   "// "
                   "M=M-D"]
    | Negate -> ["// Negate"
                 "@SP"
                 "A=M-1"
                 "M=-M"]
    | _ -> []

type MemoryAccessCommand =
    | Push of Segment * index: uint16
    | Pop of Segment * index: uint16

type SourceExpression =
    | ALCommand of ArithmeticLogicCommand
    | MACommand of MemoryAccessCommand
    | Comment of string
    | CommentedExpression of SourceExpression * comment: string // Handles when an expression in commented on the same line
    | Empty
    | UnknownExpression of ErrorMessage: string

let validArithmeticLogicCommandStrings = ["add"; "sub"; "neg"; "eq"; "gt"; "lt"; "and"; "or"; "not"]

let validMemoryAccessCommands = ["push"; "pop"]

let validSegments = ["argument"; "local"; "static"; "constant"; "this"; "that"; "pointer"; "temp"]

let arithmeticLogicCommandRegex = $"({createOptionRegex validArithmeticLogicCommandStrings})"

let memoryAccessCommandRegex = sprintf @"^(%s)\s+(%s)\s+([0-9]+)$" (createOptionRegex validMemoryAccessCommands)
                                                                   (createOptionRegex validSegments)

let stringToArithmeticLogicCommand = function
    | "add" -> Some Add
    | "sub" -> Some Subtract
    | "neg" -> Some Negate
    | "eq"  -> Some Equal
    | "gt"  -> Some GreaterThan
    | "lt"  -> Some LessThan
    | "and" -> Some And
    | "or"  -> Some Or
    | "not" -> Some Not
    | _     -> None

let stringToMemoryAccessCommand = function
    | "push" -> Some Push
    | "pop"  -> Some Pop
    | _      -> None

let stringToSegment = function
    | "argument" -> Some Argument
    | "local"    -> Some Local
    | "static"   -> Some Static
    | "constant" -> Some Constant
    | "this"     -> Some This
    | "that"     -> Some That
    | "pointer"  -> Some Pointer
    | "temp"     -> Some Temp
    | _          -> None

/// Parses the string, which represents a single line in the source assembly code, into
/// a SourceExpression. The string is trimmed before being processed.
let rec parse (str: string) =
    match str.Trim() with
    | RegexMatch arithmeticLogicCommandRegex [x] -> match stringToArithmeticLogicCommand x with
                                                    | Some cmd -> ALCommand cmd
                                                    | None     -> UnknownExpression "x"
    | RegexMatch memoryAccessCommandRegex [p; s; i] -> match stringToMemoryAccessCommand p, stringToSegment s, uint16 i with
                                                       | Some cmd, Some segment, i -> MACommand(cmd(segment, i))
                                                       | _                         -> UnknownExpression (str.Trim())
    | RegexMatch @"^//(.*)$" [comment]                -> Comment comment
    | RegexMatch @"^(.+)//(.*)$" [expr; comment]      -> CommentedExpression (parse expr, comment)
    | ""                                              -> Empty
    | _                                               -> UnknownExpression "Error"

/// Represents a line in the source file.
type SourceLine = { Source: string; LineNumber: int }

/// Parses each line in the string list into a SourceExpression * SourceLine.
let parseLines (lines: string list) =
    lines
    |> List.mapi (fun index element -> parse element, {Source=element; LineNumber=index})
    |> List.filter (function | Empty, _ -> false | _ -> true)

let translate sourceExpression filename =
    match sourceExpression with
    | ALCommand command -> translateArithmeticLogicCommand command
    | MACommand(Push(segment, index)) -> push segment index filename
    | MACommand(Pop(segment, index)) -> pop segment index filename
    | _ -> []

let emitAssembly filename (expressions: (SourceExpression * SourceLine) list) =
    List.collect (fun x -> (translate (fst x) filename)) expressions

let vmTranslate (filePath: string) =
    let filename = System.IO.Path.GetFileNameWithoutExtension(filePath)
    filePath
    |> readLines
    |> parseLines
    |> emitAssembly filename

let vmTranslateToFile (filePath: string) =
    let directory = System.IO.Path.GetDirectoryName(filePath)
    let filename = System.IO.Path.GetFileNameWithoutExtension(filePath)
    let hackFilePath = System.IO.Path.Combine(directory, filename + ".asm")
    filePath
    |> readLines
    |> parseLines
    |> emitAssembly filename
    |> (fun x -> System.IO.File.WriteAllLines(hackFilePath, x))