module Nand2Tetris.Tests.Assembler

open Xunit
open FsUnit.Xunit
open Assembler.Types
open Assembler.Core

[<Fact>]
let ``Valid symbols`` () =
    let expected = [true; true; false; true; true; true]
    let inputs = ["Test"; "_Test"; "1Test"; "$asdf"; ".lAbEl._"; ":another1"]
    let actual = List.map isValidSymbolString inputs
    expected |> should equal actual

[<Fact>]
let ``Commented expressions`` () =
    let sourceLines = ["M=1  // i=1"; "D;JGT  // if (i-100)>0 goto END"]
    let parsedExpressions =
        [CommentedExpression( CInstruction({ Dest=M; Comp=One; Jump=NULL }), "i=1");
         CommentedExpression( CInstruction({ Dest=Destination.NULL; Comp=Computation.D; Jump=JGT }), "if (i-100)>0 goto END")]
    List.map parse sourceLines |> should equal parsedExpressions

[<Fact>]
let ``Labels`` () =
    let sourceLines = ["(LOOP)"; "(END)"; "(INFINITE_LOOP)"; "(loop)"; "(end)"]
    let parsedExpressions =
        List.map (fun x -> LInstruction(Label(Symbol x))) ["LOOP"; "END"; "INFINITE_LOOP"; "loop"; "end"]
    List.map parse sourceLines |> should equal parsedExpressions