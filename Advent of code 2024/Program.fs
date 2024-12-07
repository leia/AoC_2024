open System
open Advent_of_code_2024


[<EntryPoint>]
let main argv =
    let stopWatch = System.Diagnostics.Stopwatch.StartNew()
    
    let filename =
        let d = DateTime.Now.Day
        $"day{d}a"
        //"day15b"
        
    let test () =
        InputHandler.readTestFile filename 
        |> Day7.firstPart
         
    let first () =
        InputHandler.readFile filename 
        |> Day7.firstPart 
        
    let second () =
        InputHandler.readFile filename 
        |> Day7.secondPart
        
    printfn "%A" (first())
    
    stopWatch.Stop()
    printfn "%f" stopWatch.Elapsed.TotalSeconds
    0 // return an integer exit code