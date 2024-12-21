#load "grid.fsx"
#load "algos.fsx"
#load "utils.fsx"
#load "sequtils.fsx"

let parse = Grid.parse

let validMovesNoCheat grid p =
    Grid.validNeighbours grid p
    |> Seq.filter (fun q -> (Grid.get grid q) <> '#')

let hopDistance grid start =
    let hops = Array2D.create (Array2D.length1 grid) (Array2D.length2 grid) None
    let mutable bfsData = [start]
    let mutable depth = 0
    while not (List.isEmpty bfsData) do
        bfsData <-
            bfsData
            |> List.filter (fun p -> Grid.get hops p |> Option.isNone)
            |> List.map (fun p ->
                Grid.set hops p (Some depth)
                p)
            |> List.collect (fun p -> validMovesNoCheat grid p |> List.ofSeq)
            |> List.distinct
        depth <- depth + 1
    hops

let displayHops hops =
    hops
    |> Array2D.map (fun h ->
        match h with
        | Some d -> string d
        | None -> ".")
    |> printfn "%A"

let cheat grid pos =
    let moves1 =
        Grid.validNeighbours grid pos
        |> Seq.filter (fun newPos -> (Grid.get grid newPos) = '#')
    let moves2 =
        moves1
        |> Seq.collect (validMovesNoCheat grid)
    moves2

let part1Shared grid =
    let start = Grid.findTile ((=) 'S') grid
    let goal = Grid.findTile ((=) 'E') grid
    let hopsFromStart = hopDistance grid start
    let hopsToEnd = hopDistance grid goal
    let noCheatLen = Grid.get hopsToEnd start |> Option.get
    printfn "Time to beat: %i" noCheatLen
    hopsFromStart
    |> Grid.enumerate
    |> Seq.choose (fun (p, d) -> d |> Option.map (fun dist -> (p, dist)))
    |> Seq.collect (fun (p, d) ->
        cheat grid p
        |> Seq.map (fun q -> d + 2 + (Grid.get hopsToEnd q |> Option.get)))
    |> Seq.map (fun d -> noCheatLen - d)
    |> Seq.filter (fun save -> save > 0)

let part1Test grid =
    part1Shared grid
    |> Seq.countBy id
    |> List.ofSeq
    |> List.sortBy fst

let part1Real grid =
    part1Shared grid
    |> Seq.filter (fun save -> save >= 100)
    |> Seq.length

printfn "%A" (part1Test <| parse "data/day20.test.txt")
printfn "%A" (part1Real <| parse "data/day20.data.txt")
// printfn "%A" (part2 <| parse "data/day20.test.txt")
// printfn "%A" (part2 <| parse "data/day20.data.txt")
