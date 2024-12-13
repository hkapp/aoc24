#load "grid.fsx"
#load "utils.fsx"

let parse fileName =
    Grid.parse fileName
    |> Array2D.map Utils.intFromChar

// Note: this BFS only works assuming that the `neighbours` function
// won't produce values already visited
let rec bfs (neighbours: 'State -> 'State seq) (startState: 'State) =
    Seq.append
        (seq { startState })
        (neighbours startState
        |> Seq.collect (bfs neighbours))

let walkable grid currPos =
    let currHeight = Grid.get grid currPos
    Grid.validNeighbours grid currPos
    |> Seq.filter (fun newPos -> (Grid.get grid newPos) = (currHeight + 1))

let hike grid startPos =
    bfs (walkable grid) startPos

let trailScore grid trailhead =
    hike grid trailhead
    |> Seq.filter (fun pos -> (Grid.get grid pos) = 9)
    |> Set.ofSeq
    |> Set.count

let trailhead scoring grid =
    grid
    |> Grid.enumerate
    |> Seq.filter (fun (pos, height) -> height = 0)
    |> Seq.map fst
    |> Seq.map (scoring grid)
    |> Seq.sum

let part1 = trailhead trailScore

let trailRating grid trailhead =
    hike grid trailhead
    |> Seq.filter (fun pos -> (Grid.get grid pos) = 9)
    |> Seq.length

let part2 = trailhead trailRating

printfn "%A" (part1 <| parse "data/day10.test.txt")
printfn "%A" (part1 <| parse "data/day10.data.txt")
printfn "%A" (part2 <| parse "data/day10.test.txt")
printfn "%A" (part2 <| parse "data/day10.data.txt")