module Advent_of_code_2024.Day17

open System
    
type State =
    {
        mutable A: int
        mutable B: int
        mutable C: int
        mutable Incr: int
        Instructions: byte[]
        mutable Result: byte ResizeArray
    }
    
let getInstruction(opcode: byte) (operand: byte) (state: State): unit  =    
    let c =
        match operand with
        | 4uy -> state.A
        | 5uy -> state.B
        | 6uy -> state.C
        | _ -> operand |> int
    let l = operand |> int
    state.Incr <- state.Incr + 2
    match opcode with
    | 0uy -> state.A <- state.A / (1 <<< c)
    | 1uy -> state.B <- state.B ^^^ l
    | 2uy -> state.B <- c &&& 7
    | 3uy -> if state.A <> 0 then state.Incr <- l
    | 4uy -> state.B <- state.B ^^^ state.C
    | 5uy ->
        let x = c &&& 7
        state.Result.Add(x |> byte)
    | 6uy -> state.B <- state.A / (1 <<< c)
    | 7uy -> state.C <- state.A / (1 <<< c)
    | _ -> failwith "Invalid opcode"
    
let parseInstructions (input: string seq) =
    let lastLine =
        input |> Seq.last
        
    let res =
        (lastLine.Split(": ") |> Array.last).Split(",") |> Array.map int
    res |> Array.toList
    
let parseRegister(input: string) =
    let res = input.Split(": ") |> Array.last
    res |> int

let firstPart(input: string seq) =
    let lines = input |> Seq.toList
    let state: State =
        {
          A = lines[0] |> parseRegister
          B = lines[1] |> parseRegister
          C = lines[2] |> parseRegister
          Incr = 0
          Instructions = input |> parseInstructions |> List.toArray |> Array.map byte
          Result = ResizeArray()
        }
        
    while state.Incr + 1 < state.Instructions.Length do
        getInstruction state.Instructions[state.Incr] state.Instructions[state.Incr+1] state
        
    state.Result.ToArray() |> Array.map int
    
let secondPart(input: string seq) = ()

