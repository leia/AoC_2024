module Advent_of_code_2024.Day10

open System

let up = (0, -1)
let down = (0, 1)
let left = (-1, 0)
let right = (1, 0)

let private getNeighbours (x, y) =
    [up; down; left; right]
    |> List.map (fun (dx, dy) -> (x + dx, y + dy))
    
type Tile =
    {
        Value: int
        Coords: int * int
        Neighbours: ((int*int) * int) list
    }
    
let private createTiles (arr: int[,]) =
    let width = arr.GetLength(0)
    let height = arr.GetLength(1)
    
    let tiles =
        [
            for x in 0..width - 1 do
                for y in 0..height - 1 do
                    let neighbours = getNeighbours (x, y)
                    let validNeighbours =
                        neighbours
                        |> List.filter (fun (nx, ny) -> nx >= 0 && nx < width && ny >= 0 && ny < height)
                        |> List.map(fun (x, y) -> (x, y), arr[x, y])
                    let value = arr[x,y]
                    { Value = value; Neighbours = validNeighbours; Coords = (x, y) }
        ]    
    tiles
    
let private parseInput(input: string seq) =
    let tiles =
        input
        |> Seq.toList
        |> List.map(fun f -> f.ToCharArray() |> Array.map(fun ch -> ch |> string |> int) )
        |> array2D
        |> createTiles
    tiles
        
let getPaths(input: string seq ) (distinct: bool) =
    let tiles = input |> parseInput
    
    let rec findPath (tile: Tile) (path: (int * int) list) (acc: (int * int) list list) =
        let p = tile.Coords :: path
        if tile.Value = 9 then
            let res = p::acc
            res
        else
            let res =
                tile.Neighbours
                |> List.where(fun (_, v) -> v = tile.Value + 1)
                |> List.where(fun (coords, _) -> path |> List.exists(fun c -> c = coords) |> not)
                |> List.map(fun (coords, _) ->
                    findPath (tiles |> List.find(fun t -> t.Coords = coords) ) p acc)
                |> List.concat                
            res
    let res = 
        tiles
        |> List.filter(fun t -> t.Value = 0)
        |> List.map(fun t ->
            let p = findPath t [] []
            if distinct then             
             p |> List.distinctBy(fun f -> f |> List.head, f |> List.last)
            else p)
        |> List.map List.length
        |> List.sum
    res
    
    
    
let firstPart(input: string seq) = 
    getPaths input true

let secondPart(input: string seq) =
    getPaths input false