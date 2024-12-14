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

let findWhere world robots predicate =
    simulateAll world robots
    |> SeqUtils.zipWithIndex
    |> Seq.choose (fun (i, sim) ->
        if predicate (Set.ofSeq sim) then
            Some i
        else
            None)

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

let matchFrame s =
    s
    |> Set.exists (fun p ->
        [0..10]
        |> List.forall (fun i ->
            (Set.contains (fst p, (snd p) + i) s) &&
            (Set.contains ((fst p) + i, snd p) s)))

(*
  x->
y
|
v
*)
let part2 world robots =
    let res =
        findWhere world robots matchFrame
        |> Seq.head
    display world robots res
    res

let testWorld = (11, 7)
printfn "%A" (part1 testWorld <| parse "data/day14.test.txt")
let realWorld = (101, 103)
printfn "%A" (part1 realWorld <| parse "data/day14.data.txt")
printfn "%A" (part2 realWorld <| parse "data/day14.data.txt")
