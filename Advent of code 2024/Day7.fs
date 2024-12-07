module Advent_of_code_2024.Day7

open System
open Helpers
open Microsoft.FSharp.Collections

let private getNumbers (input: string seq) = 
    let res =
        input
        |> Seq.map(fun f ->
            let items = f.Split(':')
            let numbers = items[1].Trim().Split(' ') |> Array.map(fun f -> f |> Int64.Parse) 
            
            items[0] |> Int64.Parse, numbers
        )
    res |> Seq.toArray
 
let operators = ["*"; "+"; "||"]

let calc (x: Int64) (y: Int64) (op: string) =
    match op with
    | "*" -> x * y
    | "+" -> x + y
    | "||" -> x.ToString() + y.ToString() |> Int64.Parse
    | _ -> failwith "Unknown operator"

let firstPart(input: string seq) =
    let numbers = getNumbers input
    let permMap =
        numbers
        |> Array.map(fun f -> f |> snd |> Array.length)
        |> Array.distinct
        |> Array.sort
        |> Array.map(fun f -> f, operators |> Seq.permutationsWithRepetition (f-1) |> Seq.toArray)
        |> Map.ofArray
        
    let result =
        numbers
        |> Array.map(fun (res, nmbs) ->            
            let opersCombinations = permMap[nmbs |> Array.length]
            let sum =
                opersCombinations
                |> Array.map(fun opers ->
                    let start = if opers[0] = "*" then 1 else 0
                    let o = [opers[0]] @ opers
                    nmbs
                    |> Array.indexed
                    |> Array.fold(fun acc (i, x) -> calc acc x o[i]) start                    
                )
            res, sum
        )
        |> Array.where(fun (r, s) -> s |> Array.contains r)
        |> Array.map fst
        |> Array.sum
    result
    
    

let secondPart(input: string seq) = () //same as the first part, just added new operator