#load "utils.fsx"

let parse fileName =
    System.IO.File.ReadLines fileName
    |> Seq.map (fun l ->
        match l.Split("-") with
        | [|a ; b|] -> (a, b))

let validStart (s: string) = s.StartsWith("t")

let sortEdge (l, r) =
    let basicSort =
        if l < r then
            (l, r)
        else
            (r, l)

    match (validStart l, validStart r) with
    | (true, false) -> (l, r)
    | (false, true) -> (r, l)
    | _ -> basicSort

let buildGraph edges =
    edges
    |> Seq.groupBy fst
    |> Seq.map (fun (v, us) -> (v, us |> Seq.map snd |> Array.ofSeq))
    |> Map.ofSeq

let edgeExists graph edge =
    let (src, dst) = sortEdge edge
    Map.tryFind src graph
    |> Option.defaultValue Array.empty
    |> Array.exists ((=) dst)

let triangles graph startVertex =
    Map.find startVertex graph
    |> Array.collect (fun n1 ->
        Map.tryFind n1 graph
        |> Option.defaultValue Array.empty
        |> Array.filter (fun n2 -> edgeExists graph (n2, startVertex))
        |> Array.map (fun n2 -> (startVertex, n1, n2)))

let part1 rawConns =
    let graph =
        rawConns
        |> Seq.map sortEdge
        |> buildGraph
    graph
    |> Map.keys
    |> Seq.filter validStart
    |> Seq.collect (triangles graph)
    |> Seq.length

let arrayScanBack folder init arr =
    Array.scanBack folder arr init

let arrayFoldBack folder init arr =
    Array.foldBack folder arr init

let isClique graph vertices =
    seq {
        for i in 0 .. (Array.length vertices - 1) do
        for j in (i+1) .. (Array.length vertices - 1) do
        yield (vertices[i], vertices[j])
    }
    |> Seq.forall (edgeExists graph)

let part2 rawConns =
    let graph =
        rawConns
        |> Seq.map sortEdge
        |> buildGraph
    rawConns
    |> Seq.collect (fun (a, b) -> List.toSeq [a ; b])
    |> Seq.distinct
    |> Array.ofSeq
    |> Array.sortWith (fun a b ->
        let initEdge = (a, b)
        let sortedEdge = sortEdge initEdge
        if initEdge = sortedEdge then
            -1
        else
            1)
    |> arrayFoldBack
        (fun (node: string) (knownCliques: Map<string, string array array>) ->
            Map.tryFind node graph
            |> Option.defaultValue Array.empty
            |> Array.collect (fun neigh -> Map.find neigh knownCliques)
            |> Array.append (Array.singleton Array.empty)
            |> Array.map (Array.append (Array.singleton node))
            |> Array.filter (isClique graph)
            |> (fun newCliques -> Map.add node newCliques knownCliques))
        Map.empty
    |> Map.values
    |> Array.ofSeq
    |> Array.collect id
    |> Array.maxBy Array.length
    |> Array.sort
    |> String.concat ","

printfn "%A" (part1 <| parse "data/day23.test.txt")
printfn "%A" (part1 <| parse "data/day23.data.txt")
printfn "%A" (part2 <| parse "data/day23.test.txt")
printfn "%A" (part2 <| parse "data/day23.data.txt")
