
let parse fileName =
    System.IO.File.ReadLines fileName
    |> Seq.map (fun s ->
        match s.Split ": " with
        | [|a; b|] ->
            let components =
                b.Split " "
                |> Array.map uint64
            (uint64 a, components)
    )

let rec genPermutations n =
    if n = 0 then
        seq { [] }
    else
        let recPerm = genPermutations (n-1)
        seq {
            for op in [(*); (+)] do
            yield!
                recPerm
                |> Seq.map (fun l -> op :: l)
        }

let evaluate components opSeq =
    List.zip (List.tail components) opSeq
    |> List.fold
        (fun accum (n, op) -> op accum n)
        (List.head components)

let satisfiable (expected, components) =
    genPermutations ((Array.length components) - 1)
    |> Seq.map (evaluate (List.ofArray components))
    |> Seq.exists ((=) expected)

let part1 equations =
    equations
    |> Seq.filter satisfiable
    |> Seq.map fst
    |> Seq.sum

printfn "%A" (part1 <| parse "data/day7.test.txt")
printfn "%A" (part1 <| parse "data/day7.data.txt")
// printfn "%A" (part2 <| parse "data/day7.test.txt")
// printfn "%A" (part2 <| parse "data/day7.data.txt")
