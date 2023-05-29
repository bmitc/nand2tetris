module Nand2Tetris.Tests.CPUSimulator.Signal

open Xunit
open FsUnit.Xunit
open Nand2Tetris.Utilities
open Nand2Tetris.CPUSimulator.Bit
open Nand2Tetris.CPUSimulator.Signal



let dffList data clock =
    dff data clock |> Seq.toList

[<Fact>]
let ``Signal version of Not`` () =
    Not' 

[<Fact>]
let ``An unitialized DFF should default to Zero`` () =
    dffList Seq.empty Seq.empty |> should equal [Zero]

[<Fact>]
let ``DFF stuff`` () =
    dff [One;  One;  Zero; Zero; Zero; Zero; One;  One;  Zero; Zero]
        [Tick; Tock; Tick; Tock; Tick; Tock; Tick; Tock; Tick]
    |> Seq.toList
    |> should equal [Zero; Zero; One; One; Zero; Zero; Zero; Zero; One; One]

//[<Fact>]
//let ``Single input of DFF`` () =
//    dffList [Zero] [Zero] |> should equal [Zero; Zero]
//    dffList [Zero] [One]  |> should equal [Zero; Zero]
//    dffList [One]  [Zero] |> should equal [Zero; Zero]
//    dffList [One]  [One]  |> should equal [Zero; Zero]

[<Fact>]
let ``SR Latch`` () =
    srLatch [Zero] [One]  |> Seq.toList |> should equal [Zero; Zero]
    srLatch [One]  [Zero] |> Seq.toList |> should equal [Zero; One]
    srLatch [One]  [One]  |> Seq.toList |> should equal [Zero; Zero]
    srLatch [Zero] [Zero] |> Seq.toList |> should equal [Zero; Zero]

[<Fact>]
let ``D Latch`` () =
    dLatch [Zero] [One]  |> Seq.toList |> should equal [Zero; Zero]
    dLatch [One]  [One]  |> Seq.toList |> should equal [Zero; One]