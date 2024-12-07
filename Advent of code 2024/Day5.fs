module Advent_of_code_2024.Day5

open System.Linq
open Helpers

let getRulesAndUpdates(input: string seq) =
    let rules = ResizeArray()
    let updates = ResizeArray()
    
    input
    |> Seq.iter(fun line ->
        if (line.Contains("|")) then
            let r = line.Split("|")
            rules.Add(r[0] |> int,r[1] |> int)
        else if (line.Contains(",")) then
            let u = line.Split(",") |> Array.map(fun f -> f |> int) |> Array.toList
            updates.Add(u)
        else ()
        )
    rules |> Seq.toList, updates |> Seq.toList
    
let getUpdatesWithRules (updates: int list list) rules =
    updates
    |> List.map(fun upd ->
        rules |> List.where(fun (x, y) -> upd.Contains(x) && upd.Contains(y))
        )

let validate (x:int, y: int) (update: Map<int, int>) =
    let yi = update |> Map.findKey(fun _ k -> k = y)
    let xi = update |> Map.findKey(fun _ k->  k = x)
    xi < yi

let checkUpdateWithRules(update: Map<int, int>) (rules: (int * int) list) =
    let r =
        rules
        |> List.map(fun (x, y) -> validate (x, y) update)
        |> List.forall(fun f -> f = true)        
    r

let firstPart(input: string seq) =
    let rules, updates = input |> getRulesAndUpdates    
    let updatesWithRules = getUpdatesWithRules updates rules
      
    
    let res =
        (updates, updatesWithRules)
        ||> List.map2(fun u r ->
            let uMap = Map.ofList (u |> List.indexed)
            checkUpdateWithRules uMap r, u)
        |> List.where(fun f -> fst f = true)
        |> List.map snd
        |> List.map(fun f -> f |> List.itemOnMedianIndex)
        |> List.sum  
    res
    
let secondPart(input: string seq) =
    let rules, updates = input |> getRulesAndUpdates
    
    let updatesWithRules =
        updates
        |> List.map(fun upd ->
            rules |> List.where(fun (x, y) -> upd.Contains(x) && upd.Contains(y)))
        
    let sorter  (update:int list) (rules:(int * int) list) =
        let sort x y =
            let r = rules |> List.where (fun (j,k) -> (j = x && k = y) || (j = y && k = x))
            
            if r |> List.contains (y,x) then 1
            else if r |> List.contains (x,y) then -1            
            else 0
            
        update |> List.sortWith sort
      
    let res =
        (updates, updatesWithRules)
        ||> List.map2(fun u r ->
            let uMap = Map.ofList (u |> List.indexed)
            checkUpdateWithRules uMap r, u, r)
        |> List.where(fun (x,_,_) -> x = false)
        |> List.map(fun (_,y,z) -> y, z)
        |> List.map(fun (u,r) -> sorter u r)
        
    let r =
        res
        |> List.map List.itemOnMedianIndex
        |> List.sum        
    r