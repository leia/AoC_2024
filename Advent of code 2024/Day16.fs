module Advent_of_code_2024.Day16

open System

type Tile =
    {
        IsStart: bool
        IsExit: bool
        Coords: int * int
        PathNeighbours: (int * int) list
    }
    
let private path = "."
let private exit = "E"
let private start = "S"

let private createTiles (arr: string[,]) =
    let width = arr.GetLength(0)
    let height = arr.GetLength(1)
    
    let tiles =
        [
            for x in 0..width - 1 do
                for y in 0..height - 1 do
                    let value = arr.[x, y]
                    if value = path || value = exit || value = start then
                        let neighbours = Helpers.List.getNeighbours (x, y)
                        let validNeighbours =
                            neighbours
                            |> List.where (fun (nx, ny) -> nx >= 0 && nx < width && ny >= 0 && ny < height)
                            |> List.where(fun (x, y) -> arr[x, y] = path || arr[x, y] = exit || arr[x, y] = start)
                            |> List.map(fun (x, y) -> (x, y))
                        {IsExit = (value = exit); IsStart = (value = start); PathNeighbours = validNeighbours; Coords = (x, y) }
        ]    
    tiles
    
let private parseInput(input: string seq) =
    let tiles =
        input
        |> Seq.toList
        |> List.map(fun f -> f.ToCharArray() |> Array.map(fun ch -> ch |> string) |> Array.toList )
        |> List.transpose
        |> array2D
        |> createTiles
    tiles
    
let private evaluate(path: (int * int) list) =
    let steps = (path |> List.length) - 1
    let turns =
        path
        |> List.map fst
        |> List.windowed 3
        |> List.where(fun triplet -> Math.Abs(triplet[1]-triplet.[0]) <> Math.Abs(triplet[1]-triplet.[2]))
    
    steps + (((turns |> List.length) + 1) * 1000)

let firstPart(input: string seq) =
    let tiles = input |> parseInput
    
    let rec findPath (tile: Tile) (path: (int * int) list) (acc: (int * int) list list) =
        let p = tile.Coords :: path
        let map = p |> List.indexed |> List.map (fun (i, v) -> (v, i)) |> Map.ofList
        if tile.IsExit then
            let res = p::acc
            res
        else
            let res =
                tile.PathNeighbours
                |> List.where(fun coords -> map |> Map.containsKey(coords) |> not)
                |> List.map(fun coords ->
                    findPath (tiles |> List.find(fun t -> t.Coords = coords) ) p acc)
                |> List.concat                
            res
            
    let paths = 
        tiles
        |> List.filter(fun t -> t.IsStart = true)
        |> List.map(fun t ->
            let p = findPath t [] []
            p
            )
        |> List.head
        
    Console.WriteLine("Paths: {0}", paths.Length)
    let res =
        paths
        |> List.map evaluate
        |> List.min
    
    res
    
    
    
    
let secondPart(input: string seq) = ()