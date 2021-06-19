module Assembler.Types


(********************************************
A-Instruction
********************************************)

/// A symbol is a string that can be referred to as if its a memory location.
type Symbol = Symbol of string

/// Memory address in the Hack CPU, represented by a 16-bit integer.
type MemoryAddress = MemoryAddress of uint16

/// An A-Instruction.
/// An A-Instruction string is defined to be "@value".
/// Value is either a symbol referring to a memory address or a memory address.
/// See Section 4.2.2.
type AInstruction =
    | AInstructionSymbol of Symbol
    | AInstructionAddress of MemoryAddress


(********************************************
C-Instruction
********************************************)

/// A computation specified by the Hack ALU instruction set.
/// See Figure 2.6 on page 37, Figure 4.3 on page 67, and page 109.
type Computation =
    | Zero      // 0
    | One       // 1
    | NegOne    // -1
    | D         // D
    | A         // A
    | BangD     // !D
    | BangA     // !A
    | NegD      // -D
    | NegA      // -A
    | DPlusOne  // D+1
    | APlusOne  // A+1
    | DMinusOne // D-1
    | AMinusOne // A-1
    | DPlusA    // D+A
    | DMinusA   // D-A
    | AMinusD   // A-D
    | DAndA     // D&A
    | DOrA      // D|A
    | M         // M
    | BangM     // !M
    | NegM      // -M
    | MPlusOne  // M+1
    | MMinusOne // M-1
    | DPlusM    // D+M
    | DMinusM   // D-M
    | MMinusD   // M-D
    | DAndM     // D&M
    | DOrM      // D|M

/// Destination(s) to put the result of a Hack ALU computation.
/// The possibilites are a register or memory address or a combination of these.
/// See Figure 4.4 on page 68 and page 110.
type Destination =
    | NULL // The value is not stored anywhere
    | M    // Memory[A]
    | D    // D register
    | MD   // Memory[A] and D register
    | A    // A register
    | AM   // A register and Memory[A]
    | AD   // A and D registers
    | AMD  // A and D registers and Memory[A]

/// What to do after a computation is completed.
/// See Figure 4.5 on page 69 and page 110.
type Jump =
    | NULL // no jump
    | JGT  // jump if out > 0
    | JEQ  // jump if out = 0
    | JGE  // jump if out >= 0
    | JLT  // jump if out < 0
    | JNE  // jump if out != 0
    | JLE  // jump if out <= 0
    | JMP  // jump

/// A C-Instruction.
/// A C-Instruction string is defined to be "dest=comp;jump".
/// Either the dest or jump fields may be empty.
/// See Section 4.2.3 on page 66.
type CInstruction = { Comp: Computation; Dest: Destination; Jump: Jump }

(* Note:
   Originally, the CInstruction was defined with options for the Dest and Jump fields
   to represent the null cases, since one of (both not both) of these fields can be
   omitted. However, this doesn't allow easy representation of the not both case, so
   now the null cases have been moved into the Destination ahd Jump types, and this
   yields cleaner code.
*)

/// A special symbol that a user defines to label destinations of jump commands.
/// A label string is defined by "(Xxx)".
/// Section Section 4.2.4 on page 69.
type Label = Label of Symbol

/// Represents a line in the source file.
type SourceLine = { Source: string; LineNumber: int }

/// Represents a parsed expression in the source file.
type SourceExpression =
    | CInstruction of CInstruction
    | AInstruction of AInstruction
    | LInstruction of Label
    | Comment of string
    | CommentedExpression of SourceExpression * comment: string // Handles when an expression in commented on the same line
    | Empty
    | UnknownExpression of ErrorMessage: string