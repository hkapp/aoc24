module Algorithms

open System.Collections.Generic

// Note: this DFS only works assuming that the `neighbours` function
// won't produce values already visited
let rec dfs (neighbours: 'State -> 'State seq) (startState: 'State) =
    Seq.append
        (seq { startState })
        (neighbours startState
        |> Seq.collect (dfs neighbours))

// Handles the visited set
// And returns paths
// Warning: returned paths are in reverse order
let dfs2 (neighbours: 'State -> 'State seq) (startState: 'State) =
    let visited = new Dictionary<'State, unit>()
    let neighbours1 currPath =
        neighbours (List.head currPath)
        |> Seq.filter (fun neigh -> not <| visited.ContainsKey(neigh))
        |> Seq.map (fun neigh ->
            visited.Add(neigh, ())
            neigh :: currPath)
    let startState1 = [startState]
    dfs neighbours1 startState1

// Note: this BFS only works assuming that the `neighbours` function
// won't produce values already visited
let rec bfs (neighbours: 'State -> 'State seq) (startState: 'State) =
    Seq.unfold
        (fun prevRound ->
            if Seq.isEmpty prevRound then
                None
            else
                let nextRound =
                    prevRound
                    |> Seq.collect neighbours
                Some (prevRound, nextRound))
        (Seq.singleton startState)
    |> Seq.collect id

// Handles the visited set
// And returns paths
// Warning: returned paths are in reverse order
let bfs2 (neighbours: 'State -> 'State seq) (startState: 'State) =
    let visited = new Dictionary<'State, unit>()
    let neighbours1 currPath =
        neighbours (List.head currPath)
        |> Seq.filter (fun neigh -> not <| visited.ContainsKey(neigh))
        |> Seq.map (fun neigh ->
            visited.Add(neigh, ())
            neigh :: currPath)
    let startState1 = [startState]
    bfs neighbours1 startState1

// Returns paths
// This function will happily produce duplicate states
// Warning: returned paths are in reverse order
let bfsPaths (neighbours: 'State -> 'State seq) (startState: 'State) =
    let neighbours1 currPath =
        neighbours (List.head currPath)
        |> Seq.map (fun neigh -> neigh :: currPath)
    let startState1 = [startState]
    bfs neighbours1 startState1

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

// Returns paths in reverse order
// For this function to work, the score must be monotonically increasing
// The nextStates function should also return only distinct values
let dijkstra (nextStates: 'State -> 'State seq) (dedupKey: 'State -> 'Key) (score: 'State -> 'Score) (startState: 'State) =
    let pq = new PriorityQueue<'State, 'Score>()
    pq.Enqueue(startState, score startState)

    let visited = new Dictionary<'Key, unit>()

    let rec step () =
        if pq.Count > 0 then
            let currState = pq.Dequeue()
            let currKey = dedupKey currState
            if not (visited.ContainsKey(currKey)) then
                visited.Add(currKey, ())
                nextStates currState
                |> Seq.iter (fun newState -> pq.Enqueue(newState, score newState))
                Some currState
            else
                step ()
        else
            None

    Seq.unfold
        (fun _ ->
            step ()
            |> Option.map (fun state -> (state, ())))
        ()

// TODO
let dijkstraPaths (nextStates: 'State -> 'State seq) (score: 'State list -> 'Score) (startState: 'State) =
    let nextStatesPaths currPath =
        nextStates (List.head currPath)
        |> Seq.map (fun newState -> newState :: currPath)
    let dedupKeyPaths = List.head
    let startPath = [startState]
    dijkstra nextStatesPaths dedupKeyPaths score startPath
