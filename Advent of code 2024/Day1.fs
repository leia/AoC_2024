module Advent_of_code_2024.Day1

open System
open System.Text.RegularExpressions
open Helpers

let getTwoLists(input: string seq) =
    let tuples =
        input 
        |> Seq.map(fun f -> Regex.Replace(f, @"\s+", " ") )
        |> Seq.map(_.Split(" "))
        |> Seq.map(fun f -> f[0] |> int, f[1] |> int)
        |> Seq.toList
        
    let list1, list2 = tuples |> List.tuplesToLists
    
    list1 |> List.sort, list2 |> List.sort
    

let firstPart (input: string seq) =
    let list1, list2 =
        input |> getTwoLists
        
    let res =
        (list1, list2)
        ||> List.map2(fun x y -> Math.Abs(x-y))
        |> List.sum
    res
    

let secondPart (input: string seq) =
    let leftList, rightList =
        input |> getTwoLists
        
    let getSimilarityScore (number: int) =
        let count = rightList |> List.where(fun f -> f = number) |> List.length
        number * count
        
    let res =
        leftList
        |> List.map getSimilarityScore
        |> List.sum
        
    res
        
    
