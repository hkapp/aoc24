module Algorithms

// Note: this BFS only works assuming that the `neighbours` function
// won't produce values already visited
let rec bfs (neighbours: 'State -> 'State seq) (startState: 'State) =
    Seq.append
        (seq { startState })
        (neighbours startState
        |> Seq.collect (bfs neighbours))

open System.Collections.Generic

let aStar (nextStates: 'State -> 'State seq) (dedupKey: 'State -> 'Key) (isFinal: 'State -> bool) (score: 'State -> 'Score) (startState: 'State) =
    let pq = new PriorityQueue<'State, 'Score>()
    pq.Enqueue(startState, score startState)
    let mutable bestScore = None
    let mutable bestState = None

    let dedup = new Dictionary<'Key, 'Score>()
    let notDuplicate candidate =
        let key = dedupKey candidate
        let newScore = score candidate
        let newIsBetter =
            match dedup.TryGetValue key with
            | (false, _) -> true
            | (true, currScore) -> newScore < currScore
        if newIsBetter then
            dedup.Remove(key)
            dedup.Add(key, newScore)
        newIsBetter

    while pq.Count > 0 do
        let currState = pq.Dequeue()
        nextStates currState
        |> Seq.filter (fun newState ->
            match bestScore with
            | Some cutoff -> (score newState) < cutoff
            | None -> true)
        |> Seq.iter (fun newState ->
            if isFinal newState then
                // the filter above guarantees that this is strictly better
                bestScore <- Some (score newState)
                bestState <- Some newState
            else if notDuplicate newState then
                pq.Enqueue(newState, score newState))
    bestState