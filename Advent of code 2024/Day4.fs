module Advent_of_code_2024.Day4

open System
open System.Numerics

let xmas = ['X'; 'M'; 'A'; 'S']

let searchForXmas (arr: char array array) (startx: int) (starty: int) =
    let len = xmas.Length    
    let bounds = arr.Length
    
    let search(fn: int -> int -> int -> bool) =
        let res =
            [0..(len-1)]
            |> List.map(fun f -> fn startx starty f)
            |> List.forall(fun f -> f = true)
        if res then 1 else 0
        
    let rv =
        if (startx+len > bounds) then 0
        else search(fun x y i -> arr.[y].[x+i] = xmas.[i])
    let rvb =
        if (startx-len+1 < 0) then 0
        else search(fun x y i -> arr.[y].[x-i] = xmas.[i])
    let rh =
        if (starty+len > bounds) then 0
        else search(fun x y i -> arr.[y+i].[x] = xmas.[i])
    let rhb =
        if (starty-len+1 < 0) then 0
        else search(fun x y i -> arr.[y-i][x] = xmas.[i])
    let rd =
        if (startx+len > bounds) || (starty+len > bounds) then 0
        else search(fun x y i -> arr.[y+i][x+i] = xmas.[i])        
    let rdb =
        if (startx-len+1 < 0) || (starty-len+1 < 0) then 0
        else search(fun x y i -> arr.[y-i][x-i] = xmas.[i])
    let ld =
        if (startx+len > bounds) || (starty-len+1 < 0) then 0
        else search(fun x y i -> arr.[y-i][x+i] = xmas.[i])
    let ldb =
        if (startx-len+1 < 0) || (starty+len > bounds) then 0
        else search(fun x y i -> arr.[y+i][x-i] = xmas.[i])
        
    let sum = rh + rhb + rv + rvb + rd + rdb + ld + ldb
    sum
 
let getGrid (input: string seq) =
    input
    |> Seq.map(fun f -> f |> Seq.toArray)
    |> Seq.toArray
    
let getStartIndices (ch: char)  (matrix: char array array) =
    matrix
    |> Array.mapi(fun y chars ->
        chars
        |> Array.indexed
        |> Array.where(fun (x, c) -> c = ch)
        |> Array.map fst
        |> Array.map(fun f -> f, y))
    |> Array.concat

let firstPart(input: string seq) =
    let matrix = getGrid input        
    let res =
        matrix
        |> getStartIndices 'X'
        |> Array.map(fun (x,y) -> searchForXmas matrix x y)
        |> Array.sum            
    res

let isMas(arr: char array array) (startx: int) (starty: int) =
    let cross = ResizeArray() 
    if (startx-1 >= 0) && (starty-1 >= 0) then cross.Add(arr.[starty-1].[startx-1]) //1
    if (startx-1 >= 0) && (starty+1 < arr.Length) then cross.Add(arr.[starty+1].[startx-1]) //2
    if (startx+1 < arr.[0].Length) && (starty-1 >= 0) then cross.Add(arr.[starty-1].[startx+1]) //3
    if (startx+1 < arr.[0].Length) && (starty+1 < arr.Length) then cross.Add(arr.[starty+1].[startx+1]) //4   
    
    let hasM = cross |> Seq.where(fun f -> f = 'M') |> Seq.toList |> List.length = 2
    let hasS = cross |> Seq.where(fun f -> f = 'S') |> Seq.toList |> List.length = 2
    hasM && hasS && ((cross[0] = cross[1] && cross[2] = cross[3]) || cross[0] = cross[2] && cross[1] = cross[3])

let secondPart(input: string seq) =    
    let matrix = getGrid input           
    let res =
        matrix
        |> getStartIndices 'M'
        |> Array.map(fun (x, y) -> isMas matrix x y)
        |> Array.where(fun f -> f = true)
        |> Array.length       
    res