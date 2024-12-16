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

// The best possible score reachable from this state
let aStarScore targetPos currState =
    currState.Score + uint64 (Grid.manhattanDistance targetPos currState.Pos)

let part1 grid =
    let endPos = findTile grid 'E'
    let bestFinalState =
        Algorithms.aStar
            (nextStates grid)
            (fun st -> (st.Pos, st.Dir))
            (fun currState -> currState.Pos = endPos)
            (aStarScore endPos)
            (startState grid)
        |> Option.get
    bestFinalState.Score

printfn "%A" (part1 <| parse "data/day16.test1.txt")
printfn "%A" (part1 <| parse "data/day16.test2.txt")
printfn "%A" (part1 <| parse "data/day16.data.txt")
// printfn "%A" (part2 <| parse "data/day16.test.txt")
// printfn "%A" (part2 <| parse "data/day16.data.txt")
