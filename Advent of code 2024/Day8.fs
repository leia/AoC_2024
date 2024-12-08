module Advent_of_code_2024.Day8

open System

open Helpers

let getAntennas(input: string seq) =
    let res =
        input
        |> Seq.toList
        |> Seq.mapi(fun y line ->
            line
            |> Seq.toList
            |> List.indexed
            |> List.where(fun (_, s) -> s |> Char.IsLetterOrDigit)
            |> List.map(fun (x, s) -> s, (x,y))
        )
        |> List.concat
        |> List.groupBy(fun (s, _) -> s)
        |> List.map(fun (ch, s) -> ch, s |> List.map snd)
        
    res

let getPairs (l: (int * int) list) =
    let pairs =
        l
        |> Seq.compinations 2
        |> Seq.toList
    pairs
    
let getAntiNodes boundX boundY (pair: (int * int) list) (getAll: bool) =    
    let res = ResizeArray()
    
    let check (x, y) =
        x >= 0 && x < boundX && y >= 0 && y < boundY && (res.Contains(x,y) |> not)
        
    if check pair[0] then res.Add pair[0]
    if check pair[1] then res.Add pair[1]
        
    let count c1 c2 i =
        let x1, y1 = c1
        let x2, y2 = c2
        let ax1, ay1 = x1 + ((x1-x2)*i), y1 + ((y1-y2)*i)
        let ax2, ay2 = x2 + ((x2-x1)*i), y2 + ((y2-y1)*i)
        if check (ax1, ay1) then res.Add(ax1, ay1)
        if check (ax2, ay2) then res.Add(ax2, ay2)
        
    match getAll with
    | false ->
        count pair[0] pair[1] 1
    | true ->
        let steps = Math.Max(boundX/2, boundY/2)
        
        [1..boundY]
        |> List.iter(fun i -> count pair[0] pair[1] i)
    res |> Seq.toList
    

let firstPart(input: string seq) =
    let antennas = getAntennas input
    let boundY = input |> Seq.length
    let boundX = input |> Seq.head |> Seq.length
    
    let antiNodes =
        antennas
        |> List.map(fun f -> f |> snd |> getPairs)
        |> List.map(fun x -> x |> List.map(fun f -> getAntiNodes boundX boundY f false) )
        |> List.concat
        |> List.concat
        |> List.distinct
    
    antiNodes |> List.length

let secondPart(input: string seq) =
    let antennas = getAntennas input
    let boundY = input |> Seq.length
    let boundX = input |> Seq.head |> Seq.length
    
    let antiNodes =
        antennas
        |> List.map(fun f -> f |> snd |> getPairs)
        |> List.concat
        |> List.map(fun x -> getAntiNodes boundX boundY x true) 
        |> List.concat
        |> List.distinct
    
    antiNodes |> List.length