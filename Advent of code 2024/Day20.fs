module Advent_of_code_2024.Day20

open System
open Helpers

type Tile =
    {
        Coords: int * int
        Neighbours: (int * int) list
    }
    
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

let getPaths (allObstacles: (int*int) list) boundX boundY start =
    let map = allObstacles |> List.indexed |> List.map(fun (i, j) -> (j, i)) |> Map.ofList
    let tiles =
        [
            for x in 0..boundX-1 do
                for y in 0..boundY-1 do
                    createTiles map x y boundX boundY
        ]
    let getNeighbors (tile: Tile)=
        let res =
            tile.Neighbours
            |> List.map(fun f -> tiles |> List.find(fun t -> t.Coords = f))
        res
        
    let start = tiles |> List.find(fun f -> f.Coords = start)
        
    let pq: DPQ.State<Tile> = [0, start] |> DPQ.ofSeq
    let paths = Graphs.dijkstra getNeighbors evaluate pq
    paths.Distances
    
    
let firstPart(input: string seq) =
    let arr =
        input
        |> Seq.indexed
        |> Seq.map(fun (y, line) ->
            line
            |> Seq.toList
            |> List.indexed            
            |> List.map(fun (x, c) -> (x, y), c))
        |> Seq.toList
        |> List.concat
        
    let boundX = input |> Seq.head |> Seq.length
    let boundY = input |> Seq.length
    let start = arr |> List.find(fun (_, c) -> c = 'S') |> fst
    let e = arr |> List.find(fun (_, c) -> c = 'E') |> fst
    let obstacles = arr |> List.filter(fun (_, c) -> c = '#') |> List.map(fun (c, _) -> c)
    
    let paths = getPaths obstacles boundX boundY start |> Map.toList |> List.sortBy snd |> List.map(fun (t, i) -> t.Coords, i)
    let noCheats = paths |> List.find(fun (c, _) -> c = e) |> snd
    
    let mutable res = 0
    let mutable res2 = 0
    
    paths |> List.iter(fun ((x1, y1), d1) ->
        paths |> List.iter(fun ((x2, y2), d2) ->
            if x1 <> x2 || y1 <> y2 then
                let d = Math.Abs(x1-x2) + Math.Abs(y1-y2)
                if d = 2 then
                    let shortcut = noCheats - Math.Abs(d1-d2) - 1 + d
                    if noCheats - shortcut >= 100 then
                        res <- res + 1
                        
                if d <= 20 then
                    let shortcut = noCheats - Math.Abs(d1-d2) - 1 + d
                    if noCheats - shortcut >= 100 then
                        res2 <- res2 + 1
            )
        )
    res/2 , res2/2                                           

let secondPart(input: string seq) = () 