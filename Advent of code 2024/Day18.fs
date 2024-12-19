module Advent_of_code_2024.Day18

open System
open Advent_of_code_2024.Helpers.List
open Helpers

type Tile =
    {
        Coords: int * int
        Neighbours: (int * int) list
    }

let getAllObstacles(input: string seq) =
    let res =
        input
        |> Seq.map(fun line -> line.Split(",") |> Seq.toArray)
        |> Seq.map(fun f -> f[0] |> int, f[1] |> int)
        |> Seq.toList
    res
    
let createTiles (obstacles: Map<int*int, int>) x y boundX boundY =
    let neighbours = 
        [
            (x+1, y)
            (x-1, y)
            (x, y+1)
            (x, y-1)
        ]
        |> List.filter(fun (nx, ny) -> nx >= 0 && nx < boundX && ny >= 0 && ny < boundY)
        |> List.filter(fun f -> not (obstacles.ContainsKey f))
    {Coords = (x, y); Neighbours = neighbours}
    
let evaluate acc (tile: Tile)  = acc + 1
    
let private bounds = 71   
let getPaths(steps: int) (allObstacles: (int*int) list) = 
    let obstacles =
        allObstacles
        |> List.take(steps)
        |> List.indexed
        |> List.map(fun (i, f) -> f, i)
        |> Map.ofList
    let tiles =
        [
            for x in 0..bounds-1 do
                for y in 0..bounds-1 do
                    createTiles obstacles x y bounds bounds
        ]
    let getNeighbors (tile: Tile)=
        let res =
            tile.Neighbours
            |> List.map(fun f -> tiles |> List.find(fun t -> t.Coords = f))
        res
        
    let start = tiles |> List.find(fun f -> f.Coords = (0,0))
        
    let pq: DPQ.State<Tile> = [0, start] |> DPQ.ofSeq
    let paths = Graphs.dijkstra getNeighbors evaluate pq
    paths.Distances
    
let firstPart(input: string seq) =
    let obstacles = getAllObstacles input
    let paths = getPaths 1024 obstacles
    
    let res = paths |> Map.toList |> List.where(fun (t, i) -> t.Coords = (bounds-1, bounds-1)) |> List.head |> snd    
    res
    
    

let secondPart(input: string seq) =
    let obstacles = getAllObstacles input
    
    let steps = 1024
    let mutable cont = true
    let mutable i = 1
    
    while cont do
        let paths = getPaths (steps+i) obstacles[0..(steps+i)]
        let res = paths |> Map.toList |> List.where(fun (t, i) -> t.Coords = (bounds-1, bounds-1)) |> List.tryHead
        
        if res.IsNone then cont <- false
        i <- i+1
        
    let res = obstacles[i-1+steps]
    res
    
    