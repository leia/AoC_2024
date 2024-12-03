module Advent_of_code_2024.InputHandler

open System.IO


let readFile (filename: string) =
    let path = $"../../../input/{filename}.txt"
    File.ReadLines(path) |> Seq.cast<string>
    
let readfileAsString (filename: string) =
    let path = $"../../../input/{filename}.txt"
    File.ReadAllText(path)

let readTestFile (filename: string) =
    let path = $"../../../testInput/{filename}.txt"
    File.ReadLines(path) |> Seq.cast<string>
    
let readTestfileAsString(filename: string) =
    let path = $"../../../testInput/{filename}.txt"
    File.ReadAllText(path)