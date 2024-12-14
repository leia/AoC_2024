open System
open Advent_of_code_2024


[<EntryPoint>]
let main argv =
    let filename =
        let d = DateTime.Now.Day
        $"day{d}a"
        //"day11a"
        
    let test () =
        InputHandler.readTestFile filename 
        |> Day14.firstPart
         
    let first () =
        InputHandler.readFile filename 
        |> Day14.firstPart 
        
    let second () =
        InputHandler.readFile filename 
        |> Day14.secondPart
       
    let stopWatch = System.Diagnostics.Stopwatch.StartNew()
    printfn "%A" (second())
    
    stopWatch.Stop()
    printfn "%f" stopWatch.Elapsed.TotalSeconds
    0 // return an integer exit code