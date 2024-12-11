module Advent_of_code_2024.Day11

open System

let private getStones(input: string seq) =
    let res =
        (input
        |> Seq.toList
        |> Seq.head).Split(' ')
        |> Array.map int64
    res
 
let private rules (maps: Map<int64,int64> * Map<int64,int64>)(stone: int64) =
    let str = (stone |> string)
    let map, state = maps
    let count = map |> Map.find stone
    let find item = state |> Map.tryFind item |> Option.defaultValue 0
    
    if stone = 0 then
        let c = 1L |> find
        let m = state.Add(1L, (c+count))
        map, m
    else if ((str |> String.length) % 2 = 0) then
        let str = stone |> string         
        let half = (str |> String.length) / 2
        let n1, n2 = str[0..(half-1)] |> int64, str[half..] |> int64
        if n1 <> n2 then
            let c1, c2 = n1 |> find, n2 |> find
            let m = state.Add(n1, (c1+count)).Add(n2, (c2+count))
            map, m
        else
            let c1 = n1 |> find
            let m = state.Add(n1, (c1+(count*2L)))
            map, m
    else
        let n = stone * 2024L
        let c = n |> find
        let m = state.Add(n, (c+count))
        map, m
        
let rec evaluate (stones: Map<int64,int64>) (i: int) (bound: int) =
    let _, res = stones |> Map.fold(fun acc k v -> rules acc k) (stones, Map.empty)
    if i = bound then res
    else
        evaluate res (i+1) bound

let firstPart(input: string seq) =
    let stones = input |> getStones |> Array.map(fun f -> f, 1L) |> Map.ofArray
    let res =
        evaluate stones 1 25
        |> Map.values
        |> Seq.sum  
    res
        
    
let secondPart(input: string seq) =
    let stones = input |> getStones |> Array.map(fun f -> f, 1L) |> Map.ofArray
    let res =
        evaluate stones 1 75
        |> Map.values
        |> Seq.sum        
    res