module Advent_of_code_2024.Helpers

module List =
    let isSorted(l: int list) =
        let pairs = l |> List.pairwise
        let isAsc = pairs |> List.forall(fun (x, y) -> x < y)
        let isDesc = pairs |> List.forall(fun (x, y) -> x > y)
        isAsc || isDesc
        
    let tuplesToLists(tuples: ('a * 'b) list): 'a list * 'b list =
        let list1 = tuples |> List.map(fun f -> f |> fst) 
        let list2 = tuples |> List.map(fun f -> f |> snd) 
        list1, list2
        
    let medianIndex input = 
        let sorted = [0..(input |>Seq.length)-1]
        let m1,m2 = 
            let len = sorted.Length-1 |> float
            len/2. |> floor |> int, len/2. |> ceil |> int 
        let res = (sorted.[m1] + sorted.[m2] |> float)/2. |> int
        (input |> Seq.toList)[res]