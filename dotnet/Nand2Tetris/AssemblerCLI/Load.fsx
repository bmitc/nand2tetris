// #load @"C:\Users\bmitc\Documents\GitHub\nand2tetris\dotnet\Nand2Tetris\AssemblerCLI\Load.fsx"

open System
open System.IO

let homeDirectory = Environment.GetFolderPath(Environment.SpecialFolder.UserProfile)

#r @"C:\Users\bmitc\.nuget\packages\argu\6.0.0\lib\netstandard2.0\Argu.dll"
#load @"C:\Users\bmitc\Documents\GitHub\nand2tetris\dotnet\Nand2Tetris\Assembler\Utilities.fs"
#load @"C:\Users\bmitc\Documents\GitHub\nand2tetris\dotnet\Nand2Tetris\Assembler\Core.fs"