module VirtualMachine.Core

type Data = Data of int16

type ArithmeticLogicCommand =
    | Add
    | Subtract
    | Negate
    | Equal
    | GreaterThan
    | LessThan
    | And
    | Or
    | Not

type Segment =
    | Argument
    | Local
    | Static
    | Constant
    | This
    | That
    | Pointer
    | Temporary

type MemoryAccessCommand =
    | Push of Segment * index: int
    | Pop of Segment * index: int

type Stack = Stack of Data list

exception EmptyStack of message: string

let push element (Stack data) =
    Stack (element :: data)

let pop (Stack data) =
    match data with
    | top :: rest -> (top, rest)
    | []           -> raise (EmptyStack "The stack is empty.")