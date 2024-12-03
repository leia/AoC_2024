module Advent_of_code_2024.Day3

open System.Text.RegularExpressions

let getNumbersToMul(input: string) =
    let regexp = @"mul\(\d{1,3},\d{1,3}\)"
    let muls = Regex.Matches(input, regexp) |> Seq.map(fun f -> f.Value) |> Seq.toList
    let r =
        muls
        |> List.map(fun mul ->
            Regex.Matches(mul, @"\d+")
            |> Seq.map(fun f -> f.Value |> int)
            |> Seq.toList            
            )
    r
    
    
let firstPart (input: string) =
    let res =
        input
        |> getNumbersToMul
        |> List.map(fun f -> f[0] * f[1])
        |> List.sum
    res
    
let secondPart (input: string) =
   let regexp = @"do\(\)|don't\(\)"
   let parts =
       [0] @ (Regex.Matches(input, regexp) |> Seq.map(_.Index) |> Seq.toList) @ [input.Length-1]
       |> List.pairwise
       |> List.map(fun (x,y) -> input[x..y-1])       
   let toExclude = parts |> List.where(_.StartsWith("don't()"))
    
   let r =
       (parts |> List.except toExclude)
       |> String.concat ""
       |> getNumbersToMul
       |> List.map(fun f -> f[0] * f[1])
       |> List.sum
   r