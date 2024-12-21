#load "grid.fsx"
#load "algos.fsx"
#load "utils.fsx"
#load "sequtils.fsx"

let parse = Grid.parse

let pathLength l = (List.length l) - 1

let validMovesNoCheat grid p =
    Grid.validNeighbours grid p
    |> Seq.filter (fun q -> (Grid.get grid q) <> '#')

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

let part1 grid =
    let start = Grid.findTile ((=) 'S') grid
    let goal = Grid.findTile ((=) 'E') grid
    let noCheatLen = findShortestPathLength grid start goal
    printfn "Time to beat: %i" noCheatLen
    allCheatPaths grid start goal
    |> Seq.map pathLength
    |> Seq.map (fun cheatLen -> noCheatLen - cheatLen)
    |> SeqUtils.inspect
    |> Seq.takeWhile (fun save -> save >= 100)
    |> Seq.length

printfn "%A" (parse "data/day20.test.txt")
printfn "%A" (part1 <| parse "data/day20.test.txt")
printfn "%A" (part1 <| parse "data/day20.data.txt")
// printfn "%A" (part2 <| parse "data/day20.test.txt")
// printfn "%A" (part2 <| parse "data/day20.data.txt")
