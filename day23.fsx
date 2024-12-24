let parse fileName =
    System.IO.File.ReadLines fileName
    |> Seq.map (fun l ->
        match l.Split("-") with
        | [|a ; b|] -> (a, b))

let assignIds conns =
    let ids =
        conns
        |> Seq.map (fun (a, b) -> [ a ; b ])
        |> Seq.collect Seq.ofList
        |> Seq.distinct
        |> Array.ofSeq
    Array.sortInPlace ids
    ids

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

printfn "%A" (part1 <| parse "data/day23.test.txt")
printfn "%A" (part1 <| parse "data/day23.data.txt")
//printfn "%A" (part2 <| parse "data/day23.test2.txt")
//printfn "%A" (part2 <| parse "data/day23.data.txt")
