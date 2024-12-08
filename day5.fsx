#load "arrayutils.fsx"

let splitWhere predicate s =
    let idx = Seq.findIndex predicate s
    (Seq.take idx s, Seq.skip (idx+1) s)

let parseOrdRules raw =
    raw
    |> Seq.map (
        fun (s: string) ->
            match s.Split "|" with
            | [|a; b|] -> (int a, int b)
        )
    |> Set.ofSeq

let parseUpdates raw =
    raw
    |> Seq.map (
        fun (s: string) ->
            s.Split ","
            |> Array.map int
        )

let parse fileName =
    let (rawOrdRules, rawUpdates) =
        System.IO.File.ReadLines fileName
        |> splitWhere ((=) "")
    (parseOrdRules rawOrdRules, parseUpdates rawUpdates)

let correctlyOrdered ordRules update =
    seq {
        for i in [0..(ArrayUtils.maxIndex update)] do
        for j in [(i+1)..(ArrayUtils.maxIndex update)] do
        yield (update[i], update[j])
    }
    |> Seq.forall (fun (a, b) -> not (Set.contains (b, a) ordRules))

let middle (arr: 'a array) = arr[((Array.length arr) / 2)]

let part1 (ordRules, updates) =
    updates
    |> Seq.filter (correctlyOrdered ordRules)
    |> Seq.map middle
    |> Seq.sum

// May be useful for another day
// let rec permutations arr =
//     if Array.length arr = 1 then
//         seq { List.ofArray arr }
//     else
//         [0..((Array.length arr)-1)]
//         |> Seq.collect (fun i ->
//             permutations (Array.removeAt i arr)
//             |> Seq.map (fun p -> arr[i] :: p)
//         )

let rec guidedPermutations ordRules arr =
    if Array.length arr = 1 then
        seq { arr }
    else
        guidedPermutations ordRules (Array.tail arr)
        |> Seq.collect (fun p ->
            [0..(Array.length p)]
            |> Seq.map (fun i -> Array.insertAt i (Array.head arr) p)
            |> Seq.filter (correctlyOrdered ordRules)
        )

let orderCorrectly ordRules update =
    guidedPermutations ordRules update
    |> Seq.exactlyOne

let part2 (ordRules, updates) =
    updates
    |> Seq.filter (fun u -> not (correctlyOrdered ordRules u))
    |> Seq.map (orderCorrectly ordRules)
    |> Seq.map middle
    |> Seq.sum

printfn "%A" (part1 (parse "data/day5.test.txt"))
printfn "%A" (part1 (parse "data/day5.data.txt"))
printfn "%A" (part2 (parse "data/day5.test.txt"))
printfn "%A" (part2 (parse "data/day5.data.txt"))
