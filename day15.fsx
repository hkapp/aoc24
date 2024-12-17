#load "grid.fsx"
#load "sequtils.fsx"

let parse fileName =
    let (gridLines, moveLines) =
        System.IO.File.ReadLines fileName
        |> SeqUtils.splitWhere ((=) "")
    (Grid.parseLines gridLines, Seq.collect id moveLines)

let isWideCrate c = (c = '[') || (c = ']')

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
    let rec moveWideCratesVertically mass =
        let completeCrates =
            mass
            |> Array.collect (fun pos ->
                let companionDir =
                    match Grid.get grid pos with
                    | ']' -> '<'
                    | '[' -> '>'
                let companionPos = Grid.moveUnchecked pos companionDir
                [| pos ; companionPos |])
            |> Array.distinct
        // If any crate is blocked by a wall, nothing can move
        // Otherwise, if any crate is blocked by another crate, we need to try moving those crates
        let anyBlockedBy tiles =
            completeCrates
            |> Array.exists (fun pos ->
                let nextTile =
                    Grid.moveUnchecked pos dir
                    |> Grid.get grid
                Set.contains nextTile tiles)
        let moveVertically () =
            completeCrates
            |> Array.iter (fun pos ->
                let prevEmpty = Grid.moveUnchecked pos dir
                Grid.set grid prevEmpty (Grid.get grid pos)
                Grid.set grid pos '.')
        if anyBlockedBy (Set.singleton '#') then
            None
        else if anyBlockedBy (Set.ofList ['[' ; ']']) then
            // Get the next row and try again
            completeCrates
            |> Array.map (fun pos -> Grid.moveUnchecked pos dir)
            |> Array.filter (fun pos -> isWideCrate <| Grid.get grid pos)
            |> moveWideCratesVertically
            |> Option.map moveVertically
        else
            // We're simply free to move
            moveVertically ()
            Some ()
    match Seq.head ahead |> snd with
    | '#' -> currPos
    | '.' ->
        moveRobot ()
    | 'O' ->
        moveCrates ()
        |> Option.map moveRobot
        |> Option.defaultValue currPos
    | ']' | '[' ->
        moveWideCrates [| nextPos |]
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
