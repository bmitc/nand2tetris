open System
open Argu
open Assembler.Core

type CLIArguments =
    | [<Mandatory; AltCommandLine("-f")>]
      File_Path of path : string
    | Verbose of int
with
    interface IArgParserTemplate with
        member argument.Usage =
            match argument with
            | File_Path _ -> "Required: File path of the .asm source file to assemble"
            | Verbose _   -> "Optional: Prints out debug information"

let parser = ArgumentParser.Create<CLIArguments>(programName = "assembler")

[<EntryPoint>]
let main argv =
    let arguments = parser.Parse argv
    assemble (arguments.GetResult File_Path) |> ignore
    0 // return an integer exit code