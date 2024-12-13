#load "grid.fsx"
#load "algos.fsx"
open System.Collections.Generic

let parse = Grid.parse

module MutSet =
    let New () = new Dictionary<'a, unit>()
    let contains (s: Dictionary<'a, unit>) v = s.ContainsKey v
    let add (s: Dictionary<'a, unit>) v = s.Add(v, ())

let regions garden =
    let allVisited = MutSet.New ()

    let neighbours pos =
        Grid.validNeighbours garden pos
        |> Seq.filter (fun neigh -> not (MutSet.contains allVisited neigh))
        |> Seq.filter (fun neigh -> (Grid.get garden pos) = (Grid.get garden neigh))
        |> Seq.map (fun neigh ->
            MutSet.add allVisited neigh
            neigh)

    Grid.indexes garden
    |> Seq.choose (fun pos ->
        if MutSet.contains allVisited pos then
            None
        else
            Some (Set.ofSeq <| Algorithms.bfs neighbours pos))

let area region = Set.count region

let perimeter region =
    region
    |> Seq.collect Grid.neighbours
    |> Seq.filter (fun neigh -> not (Set.contains neigh region))
    |> Seq.length

let price region = (area region) * (perimeter region)

let part1 garden =
    regions garden
    |> Seq.map price
    |> Seq.sum

printfn "%A" (part1 <| parse "data/day12.test.txt")
printfn "%A" (part1 <| parse "data/day12.data.txt")
//printfn "%A" (part2 <| parse "data/day12.test.txt")
//printfn "%A" (part2 <| parse "data/day12.data.txt")
