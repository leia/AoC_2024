module Advent_of_code_2024.Day9

open System

let private parseInput(input: string seq) =
    let lines = input |> Seq.head
    let blocks = lines |> Seq.toArray |> Array.map(fun f -> Int32.Parse(f.ToString()))
    let res = ResizeArray()
    let mutable fileId = 0
    
    blocks
    |> Array.iteri(fun i b ->
        if (i %  2 = 0) then
            [1..b |> int] |> List.iter (fun f -> res.Add(fileId))
            fileId <- fileId + 1
        else
            if (b > 0) then
                [1..b |> int] |> List.iter (fun f -> res.Add(-1))
        )
    res.ToArray()
    
    
    
let firstPart(input: string seq) = 
    let blocks = parseInput input
    let numbersToSort = blocks |> Array.where(fun f -> f > -1)
    let bLength = (numbersToSort |> Array.length)
    
    let folder  (arrays: int array * int array) (value: int) =
        let sorted, toSort = arrays
        
        let newSorted, newToSort =
            if (sorted |> Array.length = bLength)  then
                sorted, toSort
            else
                if value <> -1 then
                    let s = sorted |> Array.append [|value|]
                    s, toSort
                else
                    let s = sorted |> Array.append [|toSort |> Array.last|]
                    s, (toSort |> Array.take ((toSort |> Array.length) - 1))
        
        newSorted, newToSort
    
    let res =
       blocks
       |> Array.fold folder ([||], numbersToSort)
       |> fst
       |> Array.rev
       |> Array.mapi(fun i n -> int64(i) * int64(n))
       |> Array.sum
       
    res

let secondPart(input: string seq) = ()