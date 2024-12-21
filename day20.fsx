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

let radius n p =
    let (x, y) = p
    seq {
        for i in -n .. n do
        for j in -n .. n do
        let q = (x + i, y + j)
        if Grid.manhattanDistance p q <= n then
            yield q
    }

// Note: this function will return non cheats
// Considering that these won't be saving any time, it doesn't matter
let cheat nCheat grid pos =
    // A cheat of length n can reach any valid tile
    // whose manhattan distance is less than n away
    radius nCheat pos
    |> Seq.filter (Grid.withinBounds grid)
    |> Seq.filter (fun newPos -> (Grid.get grid newPos) <> '#')
    |> Seq.map (fun newPos -> (newPos, Grid.manhattanDistance pos newPos))

let cheatSaves allowedCheats grid =
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
        cheat allowedCheats grid p
        |> Seq.map (fun (q, cheatLen) -> d + cheatLen + (Grid.get hopsToEnd q |> Option.get)))
    |> Seq.map (fun d -> noCheatLen - d)
    |> Seq.filter (fun save -> save > 0)

let test n grid =
    cheatSaves n grid
    |> Seq.countBy id
    |> List.ofSeq
    |> List.sortBy fst

let real n grid =
    cheatSaves n grid
    |> Seq.filter (fun save -> save >= 100)
    |> Seq.length

printfn "%A" (test 2 <| parse "data/day20.test.txt")
printfn "%A" (real 2 <| parse "data/day20.data.txt")
printfn "%A" (test 20 <| parse "data/day20.test.txt")
printfn "%A" (real 20 <| parse "data/day20.data.txt")
