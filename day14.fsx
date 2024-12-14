#load "sequtils.fsx"
open System.Text.RegularExpressions

type Robot = {
    Pos:      int * int
    Velocity: int * int
}

let parseRobot s =
    let regex = Regex "p=(-?\d+),(-?\d+) v=(-?\d+),(-?\d+)"
    let matches = regex.Match s
    { Pos = (int matches.Groups[1].Value, int matches.Groups[2].Value)
      Velocity = (int matches.Groups[3].Value, int matches.Groups[4].Value)}

let parse fileName =
    System.IO.File.ReadLines fileName
    |> Seq.map parseRobot

let posMod a b =
    let m = a % b
    if m < 0 then
        m + b
    else
        m

let simulate world nSteps robot =
    let coord get =
        let shootOff = (get robot.Pos) + nSteps * (get robot.Velocity)
        posMod shootOff (get world)
    (coord fst, coord snd)

let quadrant world pos =
    let boundary get =
        ((get world) / 2)
    let comp get =
        let z = (boundary get) - (get pos)
        if z = 0 then
            None
        else if z > 0 then
            Some 1
        else
            Some (-1)
    comp fst
    |> Option.bind (fun x ->
        comp snd
        |> Option.map (fun y -> (x, y)))

let part1 world robots =
    robots
    |> Seq.map (simulate world 100)
    |> Seq.groupBy (quadrant world)
    |> Seq.filter (fun (q, s) -> Option.isSome q)
    |> Seq.map (fun (q, s) -> Seq.length s)
    |> SeqUtils.product

let simulateAll world robots =
    [0..((fst world) * (snd world))]
    |> Seq.map (fun i ->
        printf "%i " i
        robots
        |> Seq.map (simulate world i))
    |> SeqUtils.zipWithIndex

let display world robots nSteps =
    let sep () =
        for j in 0 .. ((fst world) - 1) do
            printf "-"
        printfn ""
    let state =
        robots
        |> Seq.map (simulate world nSteps)
        |> Set.ofSeq
    sep ()
    for i in 0 .. ((snd world) - 1) do
        for j in 0 .. ((fst world) - 1) do
            if Set.contains (j, i) state then
                printf "#"
            else
                printf " "
        printfn ""
    sep ()

let statisticalAnalysis sim =
    let state = Set.ofSeq sim
    let neighbours (x, y) =
        seq {
            for i in -1 .. 1 do
            for j in -1 .. 1 do
            yield Set.contains (x + i, y + j) state
        }
        |> Array.ofSeq
    let neighbourPatterns =
        state
        |> Set.toSeq
        |> Seq.countBy neighbours
    // For a same number of neighbors, we would expect each pattern to appear equally
    let shouldBeEquallyLikely: (int * (int seq)) seq =
        neighbourPatterns
        |> Seq.groupBy (fun (pattern, c) ->
            let nNeigh = Seq.filter id pattern |> Seq.length
            nNeigh)
        |> Seq.map (fun (nNeigh, indivPatterns) ->
            (nNeigh,
                indivPatterns
                |> Seq.map (fun (actualPattern, patternCount) -> patternCount)))
    shouldBeEquallyLikely
    |> Seq.map (fun (nNeigh, counts) -> (Seq.max counts) - (Seq.min counts))
    |> Seq.max

(*
  x->
y
|
v
*)
let part2 world robots =
    let res =
        simulateAll world robots
        |> Seq.maxBy (fun (i, s) -> statisticalAnalysis s)
        |> fst
    display world robots res
    res

let testWorld = (11, 7)
printfn "%A" (part1 testWorld <| parse "data/day14.test.txt")
let realWorld = (101, 103)
printfn "%A" (part1 realWorld <| parse "data/day14.data.txt")
printfn "%A" (part2 realWorld <| parse "data/day14.data.txt")
