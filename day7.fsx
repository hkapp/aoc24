
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

let rec genPermutations (opList: 'a list) n =
    if n = 0 then
        seq { [] }
    else
        let recPerm = genPermutations opList (n-1)
        seq {
            for op in opList do
            yield!
                recPerm
                |> Seq.map (fun l -> op :: l)
        }

let evaluate components opSeq =
    List.zip (List.tail components) opSeq
    |> List.fold
        (fun accum (n, op) -> op accum n)
        (List.head components)

let satisfiable opList (expected, components) =
    genPermutations opList ((Array.length components) - 1)
    |> Seq.map (evaluate (List.ofArray components))
    |> Seq.exists ((=) expected)

let calibrate opList equations =
    equations
    |> Seq.filter (satisfiable opList)
    |> Seq.map fst
    |> Seq.sum

let part1 = calibrate [(*); (+)]

let concatInts a b = uint64 (String.concat "" [string a; string b])

let part2 s = calibrate [(*); (+); concatInts] s

printfn "%A" (part1 <| parse "data/day7.test.txt")
printfn "%A" (part1 <| parse "data/day7.data.txt")
printfn "%A" (part2 <| parse "data/day7.test.txt")
printfn "%A" (part2 <| parse "data/day7.data.txt")
