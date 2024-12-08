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
        
    /// Rotates a list by one place forward.
    // let private rotate lst =
    //     List.tail lst @ [List.head lst]
    //     
    // /// Gets all rotations of a list.
    // let private getRotations lst =
    //     let rec getAll lst i = if i = 0 then [] else lst :: (getAll (rotate lst) (i - 1))
    //     getAll lst (List.length lst)
    //
    // /// Gets all permutations (without repetition) of specified length from a list.
    // let rec permutations n lst = 
    //     match n, lst with
    //     | 0, _ -> seq [[]]
    //     | _, [] -> seq []
    //     | k, _ -> lst |> getRotations |> Seq.collect (fun r -> Seq.map ((@) [List.head r]) (permutations (k - 1) (List.tail r)))
        
    /// Gets all combinations (without repetition) of specified length from a list.
    let rec compinations n lst = 
        match n, lst with
        | 0, _ -> seq [[]]
        | _, [] -> seq []
        | k, (x :: xs) -> Seq.append (Seq.map ((@) [x]) (compinations (k - 1) xs)) (compinations k xs)


        

        
        
