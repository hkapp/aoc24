#load "grid.fsx"
#load "algos.fsx"
#load "utils.fsx"
#load "sequtils.fsx"

let parse = Grid.parse

let pathLength l = (List.length l) - 1

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

let findShortestPathLength grid start goal =
    Algorithms.dijkstraPaths (validMovesNoCheat grid) List.length start
    |> Seq.find (fun path -> (List.head path) = goal)
    |> pathLength

let allCheatPaths grid start goal =
    let validMoves (p, cheatCounter, len) =
        let newLen = len + 1
        let noCheatMoves =
            let newCheatCounter =
                if cheatCounter = 1 then
                    0
                else
                    cheatCounter
            validMovesNoCheat grid p
            |> Seq.map (fun q -> (q, newCheatCounter, newLen))
        let cheatMoves =
            match cheatCounter with
            | 0 -> Seq.empty
            | 1 | 2 ->
                Grid.validNeighbours grid p
                |> Seq.filter (fun q -> (Grid.get grid q) = '#')
                |> Seq.map (fun q -> (q, cheatCounter - 1, newLen))
        Seq.append noCheatMoves cheatMoves

    let score path = (List.length path) + (Grid.manhattanDistance (List.head path |> Utils.fst3) goal)

    Algorithms.dijkstraPaths validMoves score (start, 2, 0)
    |> Seq.map (fun path -> List.map Utils.fst3 path)
    |> Seq.filter (fun path -> (List.head path) = goal)

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
    //let moves2 =
    //    moves1
    //    |> Seq.collect (Grid.validNeighbours grid)
    //let moves3 =
    //    moves2
    //    |> Seq.collect (validMovesNoCheat grid)
    // No need to apply distinct (?)
    moves2

let part1 grid =
    let start = Grid.findTile ((=) 'S') grid
    let goal = Grid.findTile ((=) 'E') grid
    let hopsFromStart = hopDistance grid start
    displayHops hopsFromStart
    let hopsToEnd = hopDistance grid goal
    displayHops hopsToEnd
    let noCheatLen = Grid.get hopsToEnd start |> Option.get //findShortestPathLength grid start goal
    printfn "Time to beat: %i" noCheatLen
    hopsFromStart
    |> Grid.enumerate
    |> Seq.choose (fun (p, d) -> d |> Option.map (fun dist -> (p, dist)))
    |> Seq.collect (fun (p, d) ->
        cheat grid p
        //|> Seq.map (fun q -> d + 3 + (Grid.get hopsToEnd q |> Option.get)))
        |> Seq.map (fun q -> d + 2 + (Grid.get hopsToEnd q |> Option.get)))
    |> Seq.map (fun d -> noCheatLen - d)
    |> Seq.filter (fun save -> save > 0)
    //|> Seq.countBy id
    //|> List.ofSeq
    |> Seq.filter (fun save -> save >= 100)
    |> Seq.length

    //allCheatPaths grid start goal
    //|> Seq.map pathLength
    //|> Seq.map (fun cheatLen -> noCheatLen - cheatLen)
    //|> SeqUtils.inspect
    //|> Seq.takeWhile (fun save -> save >= 100)
    //|> Seq.length

printfn "%A" (parse "data/day20.test.txt")
printfn "%A" (part1 <| parse "data/day20.test.txt")
printfn "%A" (part1 <| parse "data/day20.data.txt")
// printfn "%A" (part2 <| parse "data/day20.test.txt")
// printfn "%A" (part2 <| parse "data/day20.data.txt")
