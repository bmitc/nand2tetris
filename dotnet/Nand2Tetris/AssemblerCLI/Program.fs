open System
open Argu
open Assembler.Core

type CLIArguments =
    | [<Mandatory; AltCommandLine("-f")>]
      File_Path of path : string
    | [<AltCommandLine("-v")>]
      Verbose of int
with
    interface IArgParserTemplate with
        member argument.Usage =
            match argument with
            | File_Path _ -> "Required: File path of the .asm source file to assemble"
            | Verbose _   -> "Optional: Prints out debug information"

let parser = ArgumentParser.Create<CLIArguments>(programName = "assembler")

[<EntryPoint>]
let main argv =
    try
        let arguments = parser.Parse argv
        assembleFile (arguments.GetResult File_Path) |> ignore
        0 // return an integer exit code
    with
        | :? ArguParseException as ex -> printfn "%s" ex.Message
                                         0
        | ex -> printfn "%s" ex.Message
                1