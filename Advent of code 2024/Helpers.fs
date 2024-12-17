module Advent_of_code_2024.Helpers

open System
open FSharpx.Collections

module Math =
    let rec binomialCoefficient(n: int, k: int) =
        if (k > n) then failwith "k must be less than or equal to n"
        else if (k = 0) then 1
        else if (k > n/2) then binomialCoefficient(n, n-k)
        else (n * binomialCoefficient(n-1, k-1)) / k
        
    let median (min: int) (max: int) = (min + max) / 2

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
        
    let private up = (0, -1)
    let private down = (0, 1)
    let private left = (-1, 0)
    let private right = (1, 0)

    let getNeighbours (x, y) =
        [up; down; left; right]
        |> List.map (fun (dx, dy) -> (x + dx, y + dy))
        
        
module Seq =
    // Generates the cartesian outer product of a list of sequences LL
    let rec private outerProduct = function
        | [] -> Seq.singleton []
        | L::Ls -> L |> Seq.collect (fun x -> 
                    outerProduct Ls |> Seq.map (fun L -> x::L))

    // Generates all n-element combination from a list items
    let permutationsWithRepetition n items = 
        List.replicate n items |> outerProduct
        
    let rec combinations n lst = 
        match n, lst with
        | 0, _ -> seq [[]]
        | _, [] -> seq []
        | k, (x :: xs) -> Seq.append (Seq.map ((@) [x]) (combinations (k - 1) xs)) (combinations k xs)
        
///Dijkstra Priority Queue. Combines a (priority) Heap with a visited Set so we can update priorities of a node without having to iterate over the entire heap
module DPQ =
    type State<'t> when 't: comparison =
        { Heap: Heap<int * 't>
          Visited: Set<'t>
          Distances: Map<'t, int> }

    let private heapOf s = Heap.ofSeq false s

    let ofSeq s =
        { Heap = heapOf s
          Visited = Set.empty
          Distances = Map.empty }

    let rec tryUncons pq =
        pq.Heap
        |> Heap.tryUncons
        |> Option.bind (fun ((d, h), t) ->
            if pq.Visited |> Set.contains h then
                tryUncons { pq with Heap = t }
            else
                ((d, h),
                 { Visited = pq.Visited |> Set.add h
                   Distances = pq.Distances |> Map.add h d
                   Heap = t })
                |> Some)

    let visited x pq = pq.Visited |> Set.contains x

    let updateDistances updates pq =
        let unvisited =
            updates
            |> List.filter (fun n -> pq |> visited (snd n) |> not)

        { pq with Heap = pq.Heap |> Heap.merge (heapOf unvisited) }

module Graphs =
    ///Immutable version of Dijkstra's shortest path algorithm
    ///https://en.wikipedia.org/wiki/Dijkstra%27s_algorithm#Pseudocode
    let rec dijkstra neighbors evaluate (pq: DPQ.State<'a>) =
        match pq |> DPQ.tryUncons with
        | None -> pq
        | Some ((dist, coord), pqrest) ->
            let neighbours = neighbors coord
            let costed = neighbours |> List.map (fun n -> (evaluate dist n, n))
            let nextpq = pqrest |> DPQ.updateDistances costed

            dijkstra neighbors evaluate nextpq


        

        
        
