module Assembler.Core

open System.Text.RegularExpressions
open Types
open Utilities
open Nand2Tetris.Utilities

/// The predefined symbols in the Hack CPU and their RAM addresses.
/// See page 110.
let predefinedSymbolTable =
    mapKeyAndValue (fun (key, value) -> (Symbol key, MemoryAddress value))
        (Map ["SP",     0us;
              "LCL",    1us;
              "ARG",    2us;
              "THIS",   3us;
              "THAT",   4us;
              "R0",     0us;
              "R1",     1us;
              "R2",     2us;
              "R3",     3us;
              "R4",     4us;
              "R5",     5us;
              "R6",     6us;
              "R7",     7us;
              "R8",     8us;
              "R9",     9us;
              "R10",    10us;
              "R11",    11us;
              "R12",    12us;
              "R13",    13us;
              "R14",    14us;
              "R15",    15us;
              "SCREEN", 16384us;
              "KBD",    24576us])

/// Translate a computation to binary: <a c1 c2 c3 c4 c5 c6>
/// See Figure 4.3 on page 67 and page 109.
let translateComputation = function
    | Zero          -> "0101010"
    | One           -> "0111111"
    | NegOne        -> "0111010"
    | Computation.D -> "0001100"
    | Computation.A -> "0110000"
    | BangD         -> "0001101"
    | BangA         -> "0110001"
    | NegD          -> "0001111"
    | NegA          -> "0110011"
    | DPlusOne      -> "0011111"
    | APlusOne      -> "0110111"
    | DMinusOne     -> "0001110"
    | AMinusOne     -> "0110010"
    | DPlusA        -> "0000010"
    | DMinusA       -> "0010011"
    | AMinusD       -> "0000111"
    | DAndA         -> "0000000"
    | DOrA          -> "0010101"
    | Computation.M -> "1110000"
    | BangM         -> "1110001"
    | NegM          -> "1110011"
    | MPlusOne      -> "1110111"
    | MMinusOne     -> "1110010"
    | DPlusM        -> "1000010"
    | DMinusM       -> "1010011"
    | MMinusD       -> "1000111"
    | DAndM         -> "1000000"
    | DOrM          -> "1010101"

/// Translate a destination to binary: <d1 d2 d3>
/// See Figure 4.4 on page 68 and page 110.
let translateDestination = function
    | Destination.NULL -> "000"
    | M    -> "001"
    | D    -> "010"
    | MD   -> "011"
    | A    -> "100"
    | AM   -> "101"
    | AD   -> "110"
    | AMD  -> "111"

/// Translate a jump to binary: <j1 j2 j3>
/// See Figure 4.5 on page 69 and page 110.
let translateJump = function
    | NULL -> "000"
    | JGT  -> "001"
    | JEQ  -> "010"
    | JGE  -> "011"
    | JLT  -> "100"
    | JNE  -> "101"
    | JLE  -> "110"
    | JMP  -> "111"

/// Translates the C-Instruction to a string representing a 16-bit binary number.
// See Section 4.2.3 and Figure 4.3 and page 109.
let translateCInstruction {Comp = c; Dest = d; Jump = j} =
    "111" + (translateComputation c) + (translateDestination d) + (translateJump j)
    
/// Match a possible computation string to a Computation option
let compStringToComputation = function
    | "0"    -> Some Zero
    | "1"    -> Some One
    | "-1"   -> Some NegOne
    | "D"    -> Some Computation.D
    | "A"    -> Some Computation.A
    | "!D"   -> Some BangD
    | "!A"   -> Some BangA
    | "-D"   -> Some NegD
    | "-A"   -> Some NegA
    | "D+1"  -> Some DPlusOne
    | "A+1"  -> Some APlusOne
    | "D-1"  -> Some DMinusOne
    | "A-1"  -> Some AMinusOne
    | "D+A"  -> Some DPlusA
    | "D-A"  -> Some DMinusA
    | "A-D"  -> Some AMinusD
    | "D&A"  -> Some DAndA
    | "D|A"  -> Some DOrA
    | "M"    -> Some Computation.M
    | "!M"   -> Some BangM
    | "-M"   -> Some NegM
    | "M+1"  -> Some MPlusOne
    | "M-1"  -> Some MMinusOne
    | "D+M"  -> Some DPlusM
    | "D-M"  -> Some DMinusM
    | "M-D"  -> Some MMinusD
    | "D&M"  -> Some DAndM
    | "D|M"  -> Some DOrM
    | _      -> None

/// Match a possible destination string to a Destination option
let destStringToDestination = function
    | "M"   -> Some M
    | "D"   -> Some D
    | "MD"  -> Some MD
    | "A"   -> Some A
    | "AM"  -> Some AM
    | "AD"  -> Some AD
    | "AMD" -> Some AMD
    | ""    -> Some Destination.NULL
    | _     -> None

/// Match a possible jump string to a Jump option
let jumpStringToJump = function
    | "JGT" -> Some JGT
    | "JEQ" -> Some JEQ
    | "JGE" -> Some JGE
    | "JLT" -> Some JLT
    | "JNE" -> Some JNE
    | "JLE" -> Some JLE
    | "JMP" -> Some JMP
    | ""    -> Some Jump.NULL
    | _     -> None

/// List of valid strings for the computation field in a C-Instruction.
/// See Figure 4.3.
let validComputationStrings =
   ["0";
    "1";
    "-1";
    "D";
    "A";
    "!D";
    "!A";
    "-D";
    "-A";
    "D+1";
    "A+1";
    "D-1";
    "A-1";
    "D+A";
    "D-A";
    "A-D";
    "D&A";
    "D|A";
    "M";
    "!M";
    "-M";
    "M+1";
    "M-1";
    "D+M";
    "D-M";
    "M-D";
    "D&M";
    "D|M"]

/// List of valid strings for the destination field in a C-Instruction.
/// See Figure 4.4.
let validDestinationStrings = ["M"; "D"; "MD"; "A"; "AM"; "AD"; "AMD"]

/// List of valid strings for the jump field in a C-Instruction.
/// See Figure 4.5.
let validJumpStrings = ["JGT"; "JEQ"; "JGE"; "JLT"; "JNE"; "JLE"; "JMP"]

/// Regular expression pattern for testing for a C-Instruction.
/// C-Instructions are of the form "dest=comp;jump".
/// See page 66.
let cInstructionRegex = sprintf @"^(%s)?=?(%s){1};?(%s)?$" (createOptionRegex validDestinationStrings)
                                                           (createOptionRegex validComputationStrings)
                                                           (createOptionRegex validJumpStrings)

/// Regular expression pattern for testing if a symbol string is valid or not.
/// A user-defined symbol can be any sequence of letters, digits, underscore (_),
/// dot (.), dollar sign ($), and colon (:) that does not begin with a digit.
/// See page 108.
let symbolRegex = @"([A-Z|a-z|_|\.|\$|:]{1}[A-Z|a-z|_|\.|\$|:|0-9]*)"
// Note that the symbolRegex does not fix the regex to the front of the string. This is
// because this regex needs to match in a few different ways, such as "@symbol" or "(symbol)".

/// Regular expression pattern for a label. It tries to match value in "(value)"
/// to a symbol.
let labelRegex = @"\(" + symbolRegex + @"\)$"

/// Regular expression pattern for testing for an A-Instruction that references
/// a symbol and not a memory address.
/// A-Instructions are of the form "@value" and this regular expression tries to
/// match "@label".
/// See Section 4.2.2 on page 64 and page 109.
let aInstructionRegex = "^@" + symbolRegex + "$"

/// Determines if the string represents a valid symbol string.
/// A user-defined symbol can be any sequence of letters, digits, underscore (_),
/// dot (.), dollar sign ($), and colon (:) that does not begin with a digit.
/// See Section 6.2.1.
let isValidSymbolString (str: string) =
    Regex.Match(str, "^" + symbolRegex, RegexOptions.Compiled).Success

/// Generic partial active pattern that will match a regular expression and
/// then destructure into a list of the match's groups, excluding the whole group.
let (|RegexMatch|_|) pattern (input: string) =
    let m = Regex.Match (input.Trim(), pattern, RegexOptions.Compiled)
    if m.Success
    then Some (List.tail [for group in m.Groups -> group.Value.Trim()] )
    else None

/// Parses the string, which represents a single line in the source assembly code, into
/// a SourceExpression. The string is trimmed before being processed.
let rec parse (str: string) =
    match str.Trim() with
    | RegexMatch labelRegex [x]                       -> LInstruction (Label (Symbol x))
    | RegexMatch aInstructionRegex [x]                -> AInstruction (AInstructionSymbol (Symbol x))
    | RegexMatch @"^@([0-9]+)$" [x]                   -> AInstruction (AInstructionAddress (MemoryAddress (uint16 x)))
    | RegexMatch cInstructionRegex [""; _; ""]        -> UnknownExpression "Both destination and jump cannot be empty."
    | RegexMatch cInstructionRegex [_; ""; _]         -> UnknownExpression "A computation command must be provided."
    | RegexMatch cInstructionRegex [dest; comp; jump] ->
        match destStringToDestination dest, compStringToComputation comp, jumpStringToJump jump with
        | Some d, Some c, Some j -> CInstruction {Dest=d; Comp=c; Jump=j}
        | _                      -> UnknownExpression "The C-Instruction is ill-formatted or contains invalid string commands."
    | RegexMatch @"^//(.*)$" [comment]                -> Comment comment
    | RegexMatch @"^(.+)//(.*)$" [expr; comment]      -> CommentedExpression (parse expr, comment)
    | ""                                              -> Empty
    | _                                               -> UnknownExpression "Error"

/// Read a file into a list of strings, where each line in the file is a new element in the list.
let readLines filePath = System.IO.File.ReadLines filePath |> Seq.toList

/// Parses each line in the string list into a SourceExpression * SourceLine.
let parseLines (lines: string list) =
    let processLine index element = parse (element), {Source=element; LineNumber=index}
    lines
    |> List.mapi (fun index element -> parse element, {Source=element; LineNumber=index})
    |> List.filter (function | Empty, _ -> false | _ -> true)

let testParse (filePath: string) =
    let directory = System.IO.Path.GetDirectoryName(filePath)
    let filename = System.IO.Path.GetFileNameWithoutExtension(filePath)
    let parsedFilePath = System.IO.Path.Combine(directory, filename + ".parse")
    filePath
    |> readLines
    |> parseLines
    |> List.map (fun (source, _) -> string source)
    |> (fun x -> System.IO.File.WriteAllLines(parsedFilePath,x))

/// Builds the symbol table by looking through the SourceExpressions and adding each symbol reference to the
/// symbol table. The symbol string will point to the ROM address of the next assembly instruction.
let buildSymbolTable (expressions: (SourceExpression * SourceLine) list) =
    let rec loop exprs (currentROMAddress : uint16) (symbolTable : Map<Symbol, MemoryAddress>) =
        match exprs with
        | [] -> expressions, symbolTable
        | (expression, source) :: tail ->
            match expression with
            | LInstruction(Label x)           -> loop tail currentROMAddress (symbolTable.Add(x, MemoryAddress currentROMAddress))
            | AInstruction _ | CInstruction _ -> loop tail (currentROMAddress + 1us) symbolTable
            | CommentedExpression (expr, _)   -> loop ((expr, source) :: tail) currentROMAddress symbolTable
            | _                               -> loop tail currentROMAddress symbolTable
    loop expressions 0us predefinedSymbolTable

let testBuildSymbolTable (filePath: string) =
    let directory = System.IO.Path.GetDirectoryName(filePath)
    let filename = System.IO.Path.GetFileNameWithoutExtension(filePath)
    let parsedFilePath = System.IO.Path.Combine(directory, filename + ".parse")
    filePath
    |> readLines
    |> parseLines
    |> buildSymbolTable
    |> fun (_, symbolTable) -> symbolTable

/// Debug function that nicely prints a symbol table.
let printSymbolTable (symbolTable: Map<Symbol, MemoryAddress>) =
    symbolTable
    |> Map.toSeq
    |> Seq.iter (fun (Symbol x, MemoryAddress y) -> printfn "%A: %A" x y)

/// Translates the SourceExpressions into a list of instructions.
let translate (expressions: (SourceExpression * SourceLine) list, symbolTable: Map<Symbol, MemoryAddress>) =
    let rec loop exprs nextRAMAddress (symbols: Map<Symbol, MemoryAddress>) instructions =
        match exprs with
        | [] -> instructions, symbols
        | (sourceExpr, sourceLine) :: tail ->
            match sourceExpr with
            | CInstruction cInstr -> loop tail nextRAMAddress symbols ((translateCInstruction cInstr) :: instructions)
            | AInstruction (AInstructionAddress (MemoryAddress location)) -> loop tail
                                                                                  nextRAMAddress
                                                                                  symbols
                                                                                  (("0" + (binaryToString (int location) 15)) :: instructions)
            | AInstruction (AInstructionSymbol symbol) ->
                match symbols.TryFind symbol with
                | Some (MemoryAddress location) -> loop tail
                                                        nextRAMAddress
                                                        symbols
                                                        (("0" + (binaryToString (int location) 15)) :: instructions)
                | None                          -> loop tail
                                                        (nextRAMAddress + 1us)
                                                        (symbols.Add(symbol, MemoryAddress nextRAMAddress))
                                                        (("0" + (binaryToString (int nextRAMAddress) 15)) :: instructions)
            | CommentedExpression (expr, _)     -> loop ((expr, sourceLine) :: tail) nextRAMAddress symbols instructions
            | _ -> loop tail nextRAMAddress symbols instructions
    let output = loop expressions 16us symbolTable []
    (List.rev (fst output), snd output)

/// Assembles the assembly source into a sequence of 16-bit binary instructions.
/// See Section 4.2.6 on page 71.
let assemble (source: string list) =
    source
    |> parseLines
    |> buildSymbolTable
    |> translate

/// Assembles the assembly source file into a sequence of 16-bit binary instructions.
/// If the source file is named <name>.asm, then the instructions are saved in <name>.hack.
/// See Section 4.2.6 on page 71.
let assembleFile (filePath: string) =
    let directory = System.IO.Path.GetDirectoryName(filePath)
    let filename = System.IO.Path.GetFileNameWithoutExtension(filePath)
    let hackFilePath = System.IO.Path.Combine(directory, filename + ".hack")
    filePath
    |> readLines
    |> parseLines
    |> buildSymbolTable
    |> translate
    |> (fun x -> System.IO.File.WriteAllLines(hackFilePath,fst x)
                 snd x)