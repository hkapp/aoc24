#load "sequtils.fsx"

let parseInputs lines =
    lines
    |> Seq.map (fun (s: string) ->
        match s.Split(": ") with
        | [| a ; b |] -> (a, int b))
    |> Map.ofSeq

type Gate =
    { left:   string
      right:  string
      op:     string
      exec:   int -> int -> int
      output: string }

let exec op =
    match op with
    | "AND" -> (&&&)
    | "OR"  -> (|||)
    | "XOR" -> (^^^)

let parseWires lines =
    lines
    |> Seq.map (fun (s: string) ->
        match s.Split(" ") with
        | [| left ; op ; right ; arrow ; res |] ->
            { left = left
              right = right
              op = op
              exec = exec op
              output = res } )
    |> Array.ofSeq

let parse fileName =
    let (inputs, wires) =
        System.IO.File.ReadLines fileName
        |> SeqUtils.splitWhere ((=) "")
    (parseInputs inputs, parseWires wires)

let tryGate state gate =
    Map.tryFind gate.left state
    |> Option.bind
        (fun lval ->
            Map.tryFind gate.right state
            |> Option.map
                (fun rval ->
                    (gate.output, gate.exec lval rval)))

let mergeMaps onDup map1 map2 =
    let (smallest, biggest) =
        if Map.count map1 > Map.count map2 then
            (map2, map1)
        else
            (map1, map2)
    smallest
    |> Map.fold
        (fun mergeRes k v ->
            mergeRes
            |> Map.change k
                (fun prevEntry ->
                    match prevEntry with
                    | Some v2 -> Some <| onDup k v v2
                    | None -> Some v))
        biggest

exception ValueChange of string * int * int

let propagateOnce circuit state =
    let handleOverwrite wireName oldValue newValue =
        if oldValue <> newValue then
            raise <| ValueChange(wireName, oldValue, newValue)
        else
            newValue

    circuit
    |> Array.choose (tryGate state)
    |> Map.ofArray
    |> mergeMaps handleOverwrite state

let flow circuit initState =
    initState
    |> Seq.unfold
        (fun prevState ->
            let newState = propagateOnce circuit prevState
            Some (prevState, newState))

let isZWire (wireName: string) = wireName.StartsWith("z")

let formBinaryNumber finalState =
    let zNumVal (z: string) = int z[1..]
    finalState
    |> Map.filter (fun wireName wireValue -> isZWire wireName)
    |> Map.toArray
    |> Array.sumBy (fun (name, value) -> (uint64 value) * (pown 2UL (zNumVal name)))

let stableSystem zs states =
    states
    //|> Seq.pairwise
    //|> Seq.find (fun (s1, s2) -> s1 = s2)
    //|> fst
    |> Seq.find (fun state ->
        zs
        |> Array.forall (fun z -> Map.containsKey z state))

let part1 (initState, circuit) =
    let zs =
        circuit
        |> Array.map (fun g -> g.output)
        |> Array.filter isZWire
    initState
    |> flow circuit
    |> stableSystem zs
    |> formBinaryNumber

printfn "%A" (part1 <| parse "data/day24.test1.txt")
printfn "%A" (part1 <| parse "data/day24.test2.txt")
printfn "%A" (part1 <| parse "data/day24.data.txt")
//printfn "%A" (part2 <| parse "data/day24.test.txt")
//printfn "%A" (part2 <| parse "data/day24.data.txt")
