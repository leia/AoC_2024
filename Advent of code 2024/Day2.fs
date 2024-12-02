module Advent_of_code_2024.Day2

open System
open Helpers

let getReadings(input: string seq) =
    let res =
        input |> Seq.map(fun f -> f.Split(' ') |> Array.map(fun f -> f |> int) |> Array.toList)
    res |> Seq.toList
    
let isPairSafe(pair: int * int) =
    let a,b = pair
    let r = Math.Abs(a-b)
    r >=1 && r <= 3
    
let isSafe (l: int list) =
    l |> List.isSorted
        && l |> List.pairwise |> List.map isPairSafe |> List.forall(fun f -> f = true)
    
let isSafeOrDirty(l: int list) =
    let isInputSafe = l |> isSafe
    
    if isInputSafe then true
    else
        let res =
            [0.. (l.Length-1)]
            |> List.tryFind(fun f ->
                l |> List.removeAt f |> isSafe)    
        res.IsSome
        
let firstPart(input: string seq) =
    input |> getReadings |> List.where(fun f -> f |> isSafe) |> List.length   

let secondPart(input: string seq) =
    input |> getReadings |> List.where(fun f -> f |> isSafeOrDirty) |> List.length    