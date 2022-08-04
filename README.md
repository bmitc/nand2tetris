# Nand to Tetris / Elements of Computing Systems

This repository hosts implementations of the computing system described in the
* [Nand to Tetris (aka nand2tetris)](https://www.nand2tetris.org/) project,
* [_Build a Modern Computer from First Principles: From Nand to Tetris_](https://www.coursera.org/learn/build-a-computer) Coursera course, and the
* [_Elements of Computing Systems: Building a Modern Computer from First Principles_](https://www.amazon.com/Elements-Computing-Systems-Building-Principles/dp/0262640686) book.

The project, course, and book all describe the same thing and sees one implement a CPU hardware stack by being given nothing but a nand gate and a data flip-flop (DFF). Then one implements the software stack, such as an assembler, VM, and compiler.

## Goal

The final goal is to fully implement the Hack CPU on an FPGA and then the Jack language and underlying software stack using F#, where users can develop Jack code on a computer and then download it to the FPGA board to run their code, view it on a VGA monitor, and use a USB keyboard to interact with the computer.

## Components of this repository

This repository currently consists of three components:

1. **Hardware stack implemented in the project's own HDL language**. This can be found [here](/projects) and the projects follow those found in the courses and book. An example chip is the [ALU](/projects/02/ALU.hdl):

    ```hack
    CHIP ALU {
        IN x[16], y[16], // Two 16-bit data inputs
           zx,           // Zero the x input
           nx,           // Negate the x input
           zy,           // Zero the y input
           ny,           // Negate the y input
           f,            // Function code: 1 for Add, 0 for And
           no;           // Negate the out output
        OUT out[16],     // 16-bit output
            zr,          // True iff out=0
            ng;          // True iff out<0
    
        PARTS:
            Preset(in=x, zero=zx, negate=nx, out=c0);
            Preset(in=y, zero=zy, negate=ny, out=c1);
            And16(a=c0, b=c1, out=c2);
            Add16(a=c0, b=c1, out=c3);
            Mux16(a=c2, b=c3, sel=f, out=c4);
            Not16(in=c4, out=c5);
            Mux16(a=c4, b=c5, sel=no, out=c6, out[15]=ng, out=out);
            EqZero16(in=c6, out=zr);
    }
    ```

2. **Software stack implemented in [F#](https://fsharp.org/)**.

    * The [assembler](/dotnet/Nand2Tetris/Assembler/) is completed
    * The [virtual machine (VM)](/dotnet/Nand2Tetris/VirtualMachine) is a work in progress.
    * To enable testing of the VM translator, a [simulation of the Hack CPU](/dotnet/Nand2Tetris/CPUSimulator), including all chips, is also being developed purely in F#.
  
    The completed assembler is a particularly beautiful example of using F#'s discriminated unions, records, and pattern matching for domain driven design. For example, the parser is implemented using an active pattern for regular expressions:

    ```fsharp
    type SourceExpression =
        | CInstruction of CInstruction
        | AInstruction of AInstruction
        | LInstruction of Label
        | Comment of string
        | CommentedExpression of SourceExpression * comment : string // Handles when an expression in commented on the same line
        | Empty
        | UnknownExpression of ErrorMessage : string

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
    ```
    
3. **Hardware stack impelemented using VHDL, Xilinx, and a [Digilent Nexys A7-100T](https://store.digilentinc.com/nexys-a7-fpga-trainer-board-recommended-for-ece-curriculum/)**. This is a work in progress, and currently, only the combinatorial chips are implemented so far (that is, those only using a nand gate and no flip-flops or clocks). A data flip-flop (DFF) has been implemented, and the rest of the sequential chips will follow. The VHDL code can be found [here](/vivado/nand2tetris/nand2tetris.srcs/sources_1/new). An example chip implementation is a mux:

    ```vhdl
    library IEEE;
    use IEEE.STD_LOGIC_1164.ALL;

    entity hack_mux is
        port ( a        : in  STD_LOGIC;
               b        : in  STD_LOGIC;
               selector : in  STD_LOGIC;
               output   : out STD_LOGIC);
    end hack_mux;

    architecture gate_level of hack_mux is
    
        signal s0 : STD_LOGIC;
        signal s1 : STD_LOGIC;
        signal s2 : STD_LOGIC;

    begin

        use_not:  entity work.hack_not
            port map ( input  => selector,
                       output => s0);
        use_and0: entity work.hack_and
            port map ( a      => a,
                       b      => s0,
                       output => s1);
        use_and1: entity work.hack_and
            port map ( a      => selector,
                       b      => b,
                       output => s2);
        use_or:   entity work.hack_or
            port map ( a      => s1,
                       b      => s2,
                       output => output);
    
    end gate_level;
    ```
