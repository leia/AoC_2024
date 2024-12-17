open System
open Advent_of_code_2024


[<EntryPoint>]
let main argv =
    let filename =
        let d = DateTime.Now.Day
        $"day{d}a"
        //"day16a"
        
    let test () =
        InputHandler.readTestFile filename 
        |> Day17.firstPart
         
    let first () =
        InputHandler.readFile filename 
        |> Day17.firstPart 
        
    let second () =
        InputHandler.readFile filename 
        |> Day17.secondPart
       
    let stopWatch = System.Diagnostics.Stopwatch.StartNew()
    printfn "%A" (first())
    
    stopWatch.Stop()
    printfn "%f" stopWatch.Elapsed.TotalSeconds
    0 // return an integer exit code