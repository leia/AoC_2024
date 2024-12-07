module Advent_of_code_2024.Helpers

open System

module Math =
    let rec binomialCoefficient(n: int, k: int) =
        if (k > n) then failwith "k must be less than or equal to n"
        else if (k = 0) then 1
        else if (k > n/2) then binomialCoefficient(n, n-k)
        else (n * binomialCoefficient(n-1, k-1)) / k

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
        
    let itemOnMedianIndex input = 
        let sorted = [0..(input |>Seq.length)-1]
        let m1,m2 = 
            let len = sorted.Length-1 |> float
            len/2. |> floor |> int, len/2. |> ceil |> int 
        let res = (sorted.[m1] + sorted.[m2] |> float)/2. |> int
        (input |> Seq.toList)[res]
    
    let inline containsByExistsInline value source =
        List.exists (fun x -> x = value) source
        
        
module Seq =
    // Generates the cartesian outer product of a list of sequences LL
    let rec private outerProduct = function
        | [] -> Seq.singleton []
        | L::Ls -> L |> Seq.collect (fun x -> 
                    outerProduct Ls |> Seq.map (fun L -> x::L))

    // Generates all n-element combination from a list L
    let permutationsWithRepetition n L = 
        List.replicate n L |> outerProduct
        

        
        
