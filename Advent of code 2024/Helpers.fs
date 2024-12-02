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