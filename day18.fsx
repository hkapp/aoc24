#load "grid.fsx"
#load "algos.fsx"

let parse fileName =
    System.IO.File.ReadLines fileName
    |> Seq.map (fun line ->
        match line.Split "," with
        | [| l ; r |] -> (int r, int l))

let simulate width blocks =
    let grid = Array2D.create width width '.'
    blocks
    |> Seq.iter (fun pos -> Grid.set grid pos '#')
    grid

let shortestPath grid =
    let start = (0, 0)
    let endPos = ((Array2D.length1 grid) - 1, (Array2D.length2 grid) - 1)
    let nextStates currPath =
        List.head currPath
        |> Grid.validNeighbours grid
        |> Seq.filter (fun candidate -> (Grid.get grid candidate) <> '#')
        |> Seq.map (fun nextPos -> nextPos :: currPath)
    let dedupKey = List.head
    let isFinal (currPos :: ignored) = (currPos = endPos)
    let score currPath =
        (List.length currPath) + (Grid.manhattanDistance (List.head currPath) endPos)
    Algorithms.aStar
        nextStates
        dedupKey
        isFinal
        score
        [start]
    |> Option.get
    |> List.rev

let part1 width nbytes blocks =
    blocks
    |> Seq.take nbytes
    |> simulate width
    |> shortestPath
    |> List.length
    |> (fun len -> len - 1)

printfn "%A" (part1 7 12 <| parse "data/day18.test.txt")
printfn "%A" (part1 71 1024 <| parse "data/day18.data.txt")
//printfn "%A" (part2 <| parse "data/day18.test.txt")
//printfn "%A" (part2 <| parse "data/day18.data.txt")
