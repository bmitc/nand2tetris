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

type ``Assembling mult`` () =
    
    let assemblySource =
        "// Multiplies R0 and R1 and stores the result in R2.\n\
         // (R0, R1, R2 refer to RAM[0], RAM[1], and RAM[2], respectively.)\n\
         \n\
         // Compute R2=R0*R1\n\
         \n\
         // Initialize R2 to 0\n\
             @R2   // A <- Address[R2]\n\
             M=0   // R2=0\n\
         \n\
         (LOOP)\n\
             // Initilize D to R0.\n\
             // We'll use A for the possible jump location.\n\
             @R0   // A <- Address[R0]\n\
             D=M   // D=R0\n\
             // If R0=0, then we are done multiplying\n\
             @END  // A <- END to prepare for possible jump\n\
             D;JEQ // If D=0, then jump to END\n\
             // Otherwise, add R1 to R2 once\n\
             @R1   // A <- Address[R1]\n\
             D=M   // D=R1\n\
             @R2   // A <- Address[R2]\n\
             M=D+M // R2=R1+R2\n\
             // Decrement R0 by 1\n\
             @R0   // A <- Address[R0]\n\
             M=M-1 // R0=R0-1\n\
             // Continue looping\n\
             @LOOP // A <- LOOP\n\
             0;JMP // Jump to LOOP\n\
         \n\
         (END)\n\
             @END\n\
             0;JMP"

    let assemblySourceAsList = assemblySource.Split("\n") |> Seq.toList

    [<Fact>]
    member _.``Translating mult`` () =
        let hackBinary =
            "0000000000000010\n\
             1110101010001000\n\
             0000000000000000\n\
             1111110000010000\n\
             0000000000001110\n\
             1110001100000010\n\
             0000000000000001\n\
             1111110000010000\n\
             0000000000000010\n\
             1111000010001000\n\
             0000000000000000\n\
             1111110010001000\n\
             0000000000000010\n\
             1110101010000111\n\
             0000000000001110\n\
             1110101010000111"
        let hackBinaryAsList = hackBinary.Split("\n") |> Seq.toList
        assemblySourceAsList |> assemble |> fst |> should equal hackBinaryAsList

    [<Fact>]
    member _.``Parsing mult`` () =
        assemblySourceAsList |> List.map parse
        |> should equal [Comment "Multiplies R0 and R1 and stores the result in R2.";
                         Comment "(R0, R1, R2 refer to RAM[0], RAM[1], and RAM[2], respectively.)";
                         Empty;
                         Comment "Compute R2=R0*R1";
                         Empty;
                         Comment "Initialize R2 to 0";
                         CommentedExpression (AInstruction (AInstructionSymbol (Symbol "R2")), "A <- Address[R2]");
                         CommentedExpression (CInstruction {Comp = Zero; Dest = M; Jump = NULL}, "R2=0");
                         Empty;
                         LInstruction (Label (Symbol "LOOP"));
                         Comment "Initilize D to R0.";
                         Comment "We'll use A for the possible jump location.";
                         CommentedExpression (AInstruction (AInstructionSymbol (Symbol "R0")), "A <- Address[R0]");
                         CommentedExpression (CInstruction {Comp = Computation.M; Dest = D; Jump = NULL}, "D=R0");
                         Comment "If R0=0, then we are done multiplying";
                         CommentedExpression (AInstruction (AInstructionSymbol (Symbol "END")), "A <- END to prepare for possible jump");
                         CommentedExpression (CInstruction {Comp = Computation.D; Dest = Destination.NULL; Jump = JEQ}, "If D=0, then jump to END");
                         Comment "Otherwise, add R1 to R2 once";
                         CommentedExpression (AInstruction (AInstructionSymbol (Symbol "R1")), "A <- Address[R1]");
                         CommentedExpression (CInstruction {Comp = Computation.M; Dest = D; Jump = NULL}, "D=R1");
                         CommentedExpression (AInstruction (AInstructionSymbol (Symbol "R2")), "A <- Address[R2]");
                         CommentedExpression (CInstruction {Comp = DPlusM; Dest = M; Jump = NULL}, "R2=R1+R2");
                         Comment "Decrement R0 by 1";
                         CommentedExpression (AInstruction (AInstructionSymbol (Symbol "R0")), "A <- Address[R0]");
                         CommentedExpression (CInstruction {Comp = MMinusOne; Dest = M; Jump = NULL}, "R0=R0-1");
                         Comment "Continue looping";
                         CommentedExpression (AInstruction (AInstructionSymbol (Symbol "LOOP")), "A <- LOOP");
                         CommentedExpression (CInstruction {Comp = Zero; Dest = Destination.NULL; Jump = JMP}, "Jump to LOOP");
                         Empty;
                         LInstruction (Label (Symbol "END"));
                         AInstruction (AInstructionSymbol (Symbol "END"));
                         CInstruction {Comp = Zero; Dest = Destination.NULL; Jump = JMP}]

    [<Fact>]
    member _.``Building the symbol table for mult`` () =
        assemblySourceAsList |> parseLines |> buildSymbolTable |> snd
        |> should equal (predefinedSymbolTable.Add(Symbol "LOOP", MemoryAddress 2us)
                                              .Add(Symbol "END", MemoryAddress 14us))