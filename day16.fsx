#load "grid.fsx"
#load "algos.fsx"
open System.Text.RegularExpressions

let parse = Grid.parse

type State = {
    Pos:   int * int
    Dir:   char
    Score: uint64
}

let findTile grid target =
    Grid.enumerate grid
    |> Seq.filter (fun (pos, c) -> c = target)
    |> Seq.exactlyOne
    |> fst

module Part1 =
    let startState grid =
        { Pos = findTile grid 'S'
          Dir = '>'
          Score = 0UL }

    let nextStates grid currState =
        let turnStates =
            seq { Grid.rotateLeft currState.Dir ; Grid.rotateRight currState.Dir }
            |> Seq.map (fun newDir ->
                { currState with
                    Dir = newDir
                    Score = currState.Score + 1000UL })
        let moveStates =
            seq { Grid.moveUnchecked currState.Pos currState.Dir }
            |> Seq.filter (fun newPos -> (Grid.get grid newPos) <> '#')
            |> Seq.map (fun newPos -> { currState with
                                          Pos = newPos
                                          Score = currState.Score + 1UL } )
        Seq.append turnStates moveStates

    let dedupKey st = (st.Pos, st.Dir)

    let isFinal target currState = (currState.Pos = target)

    // The best possible score reachable from this state
    let score targetPos currState =
        currState.Score + uint64 (Grid.manhattanDistance targetPos currState.Pos)

    let endPos grid = findTile grid 'E'

let part1 grid =
    let endPos = Part1.endPos grid
    Algorithms.aStar
        (Part1.nextStates grid)
        Part1.dedupKey
        (Part1.isFinal endPos)
        (Part1.score endPos)
        (Part1.startState grid)
    |> Option.map (fun st -> st.Score)
    |> Option.get

type State2 = {
    State1: State
    WentThrough: bool
}

let upgrade b st = { State1 = st ; WentThrough = b }
let downgrade st = st.State1

// Modified AStar that enforces we go through a particular point
module Part2 =
    let startState grid =
        Part1.startState grid
        |> upgrade false

    let nextStates grid midPoint currState =
        Part1.nextStates grid (downgrade currState)
        |> Seq.map (fun st1 ->
            let wasThrough = currState.WentThrough
            let nowThrough = (st1.Pos = midPoint)
            upgrade (wasThrough || nowThrough) st1)

    let dedupKey st2 = (Part1.dedupKey <| downgrade st2, st2.WentThrough)

    let isFinal target currState = currState.WentThrough && (Part1.isFinal target <| downgrade currState)

    // The best possible score reachable from this state
    let score targetPos midPoint currState =
        if currState.WentThrough then
            Part1.score targetPos (downgrade currState)
        else
            currState.State1.Score +
                uint64 (Grid.manhattanDistance midPoint currState.State1.Pos) +
                uint64 (Grid.manhattanDistance targetPos midPoint)

let part2 grid =
    let endPos = Part1.endPos grid
    let globalShortest = part1 grid

    // A point is on a shortest path if a path through that point has the same cost as the global minimum
    let isOnShortestPath midPoint =
        // Run AStar guaranteeing that we go through this particular midpoint
        let shortestThrough =
            Algorithms.aStar
                (Part2.nextStates grid midPoint)
                Part2.dedupKey
                (Part2.isFinal endPos)
                (Part2.score endPos midPoint)
                (Part2.startState grid)
            |> Option.map (fun st -> st.State1.Score)
            |> Option.get
        shortestThrough = globalShortest

    let offByTwo =
        Grid.enumerate grid
        |> Seq.filter (fun (p, c) -> c = '.')
        |> Seq.map fst
        |> Seq.map (fun p ->
            printf "%A " p
            p)
        |> Seq.filter isOnShortestPath
        |> Seq.length
    // start and end
    2 + offByTwo

printfn "%A" (part1 <| parse "data/day16.test1.txt")
printfn "%A" (part1 <| parse "data/day16.test2.txt")
printfn "%A" (part1 <| parse "data/day16.data.txt")
printfn "%A" (part2 <| parse "data/day16.test1.txt")
printfn "%A" (part2 <| parse "data/day16.test2.txt")
printfn "%A" (part2 <| parse "data/day16.data.txt")
