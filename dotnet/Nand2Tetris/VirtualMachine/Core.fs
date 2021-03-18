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
    | Temporary

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

type MemoryAccessCommand =
    | Push of Segment * index: int
    | Pop of Segment * index: int

type SourceExpression =
    | ALCommand of ArithmeticLogicCommand
    | MACommand of MemoryAccessCommand
    | Comment of string
    | CommentedExpression of SourceExpression * comment: string // Handles when an expression in commented on the same line
    | Empty
    | UnknownExpression of ErrorMessage: string

type Stack = Stack of Data list

exception EmptyStack of message: string

let validArithmeticLogicCommandStrings = ["add"; "sub"; "neg"; "eq"; "gt"; "lt"; "and"; "or"; "not"]

let validMemoryAccessCommands = ["push"; "pop"]

let validSegments = ["argument"; "local"; "static"; "constant"; "this"; "that"; "pointer"; "temp"]

let arithmeticLogicCommandRegex = createOptionRegex validArithmeticLogicCommandStrings

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
    | "temp"     -> Some Temporary
    | _          -> None

let push element (Stack data) =
    Stack (element :: data)

let pop (Stack data) =
    match data with
    | top :: rest -> (top, rest)
    | []           -> raise (EmptyStack "The stack is empty.")

/// Parses the string, which represents a single line in the source assembly code, into
/// a SourceExpression. The string is trimmed before being processed.
let rec parse (str: string) =
    match str.Trim() with
    | RegexMatch arithmeticLogicCommandRegex [x]       -> match stringToArithmeticLogicCommand x with
                                                          | Some cmd -> ALCommand cmd
                                                          | None     -> UnknownExpression "x"
    | RegexMatch arithmeticLogicCommandRegex [p; s; i] -> match stringToMemoryAccessCommand p, stringToSegment s, int i with
                                                          | Some cmd, Some segment, i -> MACommand(cmd(segment, i))
                                                          | _                         -> UnknownExpression (str.Trim())
    | RegexMatch @"^//(.*)$" [comment]                -> Comment comment
    | RegexMatch @"^(.+)//(.*)$" [expr; comment]      -> CommentedExpression (parse expr, comment)
    | ""                                              -> Empty
    | _                                               -> UnknownExpression "Error"