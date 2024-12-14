module Advent_of_code_2024.Day14

open System
open Helpers

let private boundsX = 101
let private boundsY = 103

let private getRobots(input: string seq) =
    let getCoords (s: string) =
        let c = s[2..].Split(',')
        (c[0] |> Int32.Parse, c[1] |> Int32.Parse) 
        
    let res =
        input
        |> Seq.map(fun l ->
            let parts = l.Split(' ') |> Array.map getCoords
            (parts.[0], parts.[1])
            )
    res
    
let private getNewCoord(c: int) (bounds: int) =
    if c < 0 then bounds + c
     elif c >= bounds then c - bounds
     else c
    
let rec private getNthPosition (current: (int * int) * (int *  int)) (acc: int) =
    let (px, py), (vx, vy) = current    
    let nx, ny = (getNewCoord (px + vx) boundsX), (getNewCoord (py + vy) boundsY)
    
    if (acc = 100) then
        (nx, ny)
    else
        getNthPosition ((nx, ny), (vx, vy)) (acc + 1)
        
let getNextPosition (current: (int * int) * (int *  int))  =
    let (px, py), (vx, vy) = current    
    let nx, ny = (getNewCoord (px + vx) boundsX), (getNewCoord (py + vy) boundsY)
    
    (nx, ny), (vx, vy)
    
    
let firstPart(input: string seq) =
    let robotsInit = getRobots input
    
    let robotsMoved =
        robotsInit
        |> Seq.toList
        |> List.map(fun line -> getNthPosition line 1)
    
    let mx, my = Math.median 0 boundsX, Math.median 0 boundsY
    
    let g1 = robotsMoved |> List.filter(fun (x, y) -> x < mx && y < my) |> List.length
    let g2 = robotsMoved |> List.filter(fun (x, y) -> x < mx && y > my) |> List.length
    let g3 = robotsMoved |> List.filter(fun (x, y) -> x > mx && y < my) |> List.length
    let g4 = robotsMoved |> List.filter(fun (x, y) -> x > mx && y > my) |> List.length
    let res = g1 * g2 * g3 * g4
    res

let secondPart(input: string seq) =
    let robotsInit = getRobots input |> Seq.toList
    let mutable i = 1
    
    let rec getPositions (robots: ((int * int) * (int *  int)) list) =
        let positions = robots |> List.map(fun line -> getNextPosition line)
        let distinct = positions |> List.map fst |> List.distinctBy(fun (x, y) -> x, y)
         
        if positions.Length = distinct.Length then
            i
        else
            i <- i + 1
            getPositions positions
            
    getPositions robotsInit