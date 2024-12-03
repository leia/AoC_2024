open System
open Advent_of_code_2024


[<EntryPoint>]
let main argv =
    let stopWatch = System.Diagnostics.Stopwatch.StartNew()
    
    let filename =
        let d = DateTime.Now.Day
        $"day{d}a"
        //"day15b"
        
    // let test =
    //     InputHandler.readTestfileAsString filename 
    //     |> Day3.secondPart
        
    // let first =
    //     InputHandler.readfileAsString filename 
    //     |> Day3.firstPart 
        
    let second =
        InputHandler.readfileAsString filename 
        |> Day3.secondPart
        
    printfn "%A, %A" second ()
    
    stopWatch.Stop()
    printfn "%f" stopWatch.Elapsed.TotalSeconds
    0 // return an integer exit code