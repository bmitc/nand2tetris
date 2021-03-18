module Nand2Tetris.Tests.VirtualMachine

open Xunit
open FsUnit.Xunit
open VirtualMachine.Core

[<Fact>]
let ``Parse arithmetic and logic commands`` () =
    let sourceLines = ["add"; "Sub"; " eq "; " and // test"]
    let expected = [ALCommand Add; UnknownExpression "Sub"; ALCommand Equal; CommentedExpression(ALCommand And, "test")]
    List.map parse sourceLines |> should equal expected