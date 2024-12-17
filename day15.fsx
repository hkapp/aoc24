#load "grid.fsx"
#load "sequtils.fsx"

let parse fileName =
    let (gridLines, moveLines) =
        System.IO.File.ReadLines fileName
        |> SeqUtils.splitWhere ((=) "")
    (Grid.parseLines gridLines, Seq.collect id moveLines)

let applyMove grid currPos dir =
    let ahead = Grid.lookWithIndex grid currPos dir
    let nextPos = fst (Seq.head ahead)
    let moveRobot () =
        Grid.set grid nextPos '@'
        Grid.set grid currPos '.'
        nextPos
    let findOpening () =
        ahead
        |> Seq.takeWhile (fun (pos, c) -> c <> '#')
        |> Seq.tryFind (fun (pos, c) -> c = '.')
    let moveCrates () =
        findOpening ()
        |> Option.map (fun (openSpot, c) ->
            Grid.set grid nextPos '.'
            Grid.set grid openSpot 'O')
    match Seq.head ahead |> snd with
    | '#' -> currPos
    | '.' ->
        moveRobot ()
    | 'O' ->
        moveCrates ()
        |> Option.map moveRobot
        |> Option.defaultValue currPos

let run grid moves =
    let mutable currPos =
        Grid.enumerate grid
        |> Seq.filter (fun (pos, c) -> c = '@')
        |> Seq.exactlyOne
        |> fst
    moves
    |> Seq.iter (fun dir ->
        currPos <- applyMove grid currPos dir)

let part1 (grid, moves) =
    run grid moves
    Grid.enumerate grid
    |> Seq.filter (fun (pos, c) -> c = 'O')
    |> Seq.map (fun ((x, y), c) -> 100 * x + y)
    |> Seq.sum

printfn "%A" (part1 <| parse "data/day15.test.txt")
printfn "%A" (part1 <| parse "data/day15.data.txt")
// printfn "%A" (part2 <| parse "data/day15.data.txt")
