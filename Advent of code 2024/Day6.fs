module Advent_of_code_2024.Day6

open System
open Helpers

let private getPlan(input: string seq) =
    let res =
        input
        |> Seq.map(fun f -> f |> Seq.toList)
        |> Seq.toList
    res
    
let private getObstacles(plan: char list list) =
    let res =
        plan
        |> List.indexed
        |> List.map(fun (j, l) -> l |> List.indexed |> List.where(fun (i, c) -> c = '#') |> List.map(fun (i, c) -> (i, j)))
        |> List.concat
    res
    
let private getStartingPosition(plan: char list list) =
    let possiblePositions = ['^'; '>'; 'v'; '<']
    let res =
        plan
        |> List.indexed
        |> List.map(fun (j, l) -> l |> List.indexed |> List.where(fun (i, c) -> possiblePositions |> List.contains(c)) |> List.map(fun (i, c) -> (i, j)))
        |> List.concat
        |> List.head
    res
    
let private getEmptySpaces(plan: char list list) =
    let res =
        plan
        |> List.indexed
        |> List.map(fun (j, l) -> l |> List.indexed |> List.where(fun (i, c) -> c = '.') |> List.map(fun (i, c) -> (i, j)))
        |> List.concat
    res

let move (obstacles: (int*int) list) (currentPosition: int*int) (dir: char) maxX maxY =    
    let add (p: (int * int) list ) (visited: Set<int*int>) =
        let folder (visited: Set<int*int>) (x, y) =
            let isNew = visited.Contains(x, y) |> not
            let set =
                if isNew  then visited.Add(x, y)
                else  visited
            set
                
        let newSet =
            p
            |> List.fold folder visited 
        
        ()
        let added = newSet.Count - visited.Count
        added, if added > 0 then newSet else visited
    
    let isBlocked visitedPositions cp = visitedPositions |> List.length = 1 && visitedPositions |> List.head = cp
    let rec nextMove (visitedSet: Set<int * int>) cp dir =
        let cx, cy = cp
        match dir with
        | 'U' ->
            let nextObstacle = obstacles |> List.where(fun (x, y) -> cx = x && y < cy) |> List.tryLast            
            match nextObstacle with
            | Some (nx, ny) ->
                let visitedPositions = [(ny+1) .. cy] |> List.map(fun y -> (cx, y)) 
                let added, v = add visitedPositions visitedSet
                if added > 0 then
                    nextMove v (visitedPositions |> List.head) 'R'
                else
                    let b = isBlocked visitedPositions cp |> not
                    b, v
            | None ->
                let visitedPositions = [0 .. cy] |> List.map(fun y -> (cx, y)) 
                let added, v = add visitedPositions visitedSet
                added = 0, v
        | 'D' ->
            let nextObstacle = obstacles |> List.where(fun (x, y) -> x = cx && y > cy) |> List.tryHead            
            match nextObstacle with
            | Some (nx, ny) ->
                let visitedPositions = [cy .. (ny-1)] |> List.map(fun y -> (cx, y))
                let added, v = add visitedPositions visitedSet
                if added > 0 then
                    nextMove v (visitedPositions |> List.last) 'L'
                else
                    let b = isBlocked visitedPositions cp |> not
                    b, v
            | None -> 
                let visitedPositions = [cy .. maxY] |> List.map(fun y -> (cx, y))
                let added, v = add visitedPositions visitedSet
                added = 0, v
                
        | 'L' ->
            let nextObstacle = obstacles |> List.where(fun (x, y) -> y = cy && x < cx) |> List.tryLast
            match nextObstacle with
            | Some (nx, ny) ->
                let visitedPositions = [(nx+1)..cx ] |> List.map(fun x -> (x, cy))
                let added, v = add visitedPositions visitedSet
                if added > 0 then
                    nextMove v (visitedPositions |> List.head) 'U'
                else
                    let b = isBlocked visitedPositions cp |> not
                    b, v
            | None ->
                let visitedPositions = [0..cx ] |> List.map(fun x -> (x, cy))
                let added, v = add visitedPositions visitedSet
                added = 0, v
        | 'R' -> 
            let nextObstacle = obstacles |> List.where(fun (x, y) -> y = cy && x > cx) |> List.tryHead
            match nextObstacle with
            | Some (nx, ny) ->
                let visitedPositions = [cx .. (nx-1)] |> List.map(fun x -> (x, cy))
                let added, v = add visitedPositions visitedSet
                if added > 0 then
                    nextMove v (visitedPositions |> List.last) 'D'
                else
                    let b = isBlocked visitedPositions cp |> not
                    b, v
            | None ->
                let visitedPositions = [cx .. maxX] |> List.map(fun x -> (x, cy))
                let added, v = add visitedPositions visitedSet
                added = 0, v
            
    let isLoop, visited = nextMove ([currentPosition] |> Set.ofList) currentPosition dir        
    visited |> Set.toList, isLoop
    

let firstPart (input: string seq) =
    let plan = input |> getPlan
    let boundx = ((plan |> List.head |> List.length)-1)
    let boundy = ((plan |> List.length)-1)
    let startingPosition = plan |> getStartingPosition
    let obstacles = plan |> getObstacles
    let res, _ = move obstacles startingPosition 'U' boundx boundy
    
    res |> List.length

let secondPart (input: string seq) =
    let plan = input |> getPlan
    let boundx = ((plan |> List.head |> List.length)-1)
    let boundy = ((plan |> List.length)-1)
    let startingPosition = plan |> getStartingPosition
    let obstacles = plan |> getObstacles
    let visited, _ = move obstacles startingPosition 'U' boundx boundy
    
    let res =
        visited |> List.except [startingPosition]
        |> List.map(fun f ->
            let x, y = f
            let r = obstacles @ [f] |> List.sortBy (fun (x, y) -> x, y)
            let _, s = move r startingPosition 'U' boundx boundy
            f, s
            )
        |> List.where(fun (_, f) -> f = true)
    res |> List.length