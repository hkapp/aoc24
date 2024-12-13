module Algorithms

// Note: this BFS only works assuming that the `neighbours` function
// won't produce values already visited
let rec bfs (neighbours: 'State -> 'State seq) (startState: 'State) =
    Seq.append
        (seq { startState })
        (neighbours startState
        |> Seq.collect (bfs neighbours))
