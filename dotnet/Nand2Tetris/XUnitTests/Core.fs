module Assembler.Tests

open Xunit
open Assembler.Core

[<Fact>]
let ``Valid symbols`` () =
    let expected = [true; true; false; true; true; true]
    let inputs = ["Test"; "_Test"; "1Test"; "$asdf"; ".lAbEl._"; ":another1"]
    let actual = List.map isValidSymbolString inputs
    Assert.Equal<bool list>(expected, actual)