let parse fileName =
    System.IO.File.ReadLines fileName
    |> Seq.map uint64

let mix secret x = secret ^^^ x

let prune secret = secret % 16777216UL

let pseudoRandom secret =
    let step1 n =
        mix n (n * 64UL)
        |> prune

    let step2 n =
        mix n (n / 32UL)
        |> prune

    let step3 n =
        mix n (n * 2048UL)
        |> prune

    step1 secret
    |> step2
    |> step3

let secrets initSecret =
    Seq.unfold
        (fun n -> Some (n, pseudoRandom n))
        initSecret

let secretAfter n initSecret =
    secrets initSecret
    |> Seq.item n

let part1 input =
    input
    |> Seq.map (secretAfter 2000)
    |> Seq.sum

let prices initSecret =
    secrets initSecret
    |> Seq.map (fun s -> s % 10UL)

let priceChanges initSecret =
    prices initSecret
    |> Seq.pairwise
    |> Seq.map (fun (oldPrice, newPrice) -> (int64 newPrice) - (int64 oldPrice))

let valueOf fastAccess pattern =
    fastAccess
    |> Array.sumBy (fun m ->
        Map.tryFind pattern m
        |> Option.defaultValue 0UL)

let parallelDistinct arr =
    arr
    |> Array.Parallel.groupBy id
    |> Array.Parallel.map fst

let bruteforce fastAccess =
    let validPatterns =
        fastAccess
        |> Array.Parallel.map Map.keys
        |> Array.Parallel.collect Array.ofSeq
        |> parallelDistinct

    printfn "Trying %i patterns..." (Array.length validPatterns)

    validPatterns
    |> Array.Parallel.maxBy (valueOf fastAccess)
    |> (fun bestPattern -> (bestPattern, valueOf fastAccess bestPattern))

let analyze fastAccess =
    fastAccess
    |> Array.Parallel.map Map.keys
    |> Array.Parallel.collect Array.ofSeq
    |> parallelDistinct
    |> Array.length

let part2 input =
    let buyerPatterns initSecret =
        priceChanges initSecret
        |> Seq.take 2000
        |> Seq.windowed 4

    let sellingPrices initSecret =
        prices initSecret
        |> Seq.skip 4
        |> Seq.zip (buyerPatterns initSecret)

    let fastAccess sps =
        sps
        |> Seq.fold
            (fun currMap (pattern, price) ->
                if Map.containsKey pattern currMap then
                    currMap
                else
                    Map.add pattern price currMap)
            Map.empty

    input
    |> Seq.map sellingPrices
    |> Seq.toArray
    |> Array.Parallel.map fastAccess
    |> (fun x ->
        printfn "Fast access ready"
        x)
    |> bruteforce
    //|> analyze

printfn "%A" (secrets 123UL |> Seq.take 11 |> List.ofSeq)

printfn "%A" (part1 <| parse "data/day22.test.txt")
printfn "%A" (part1 <| parse "data/day22.data.txt")

printfn "%A" (prices 123UL |> Seq.take 10 |> List.ofSeq)
printfn "%A" (priceChanges 123UL |> Seq.take 10 |> List.ofSeq)

printfn "%A" (part2 [123UL])
printfn "%A" (part2 <| parse "data/day22.test2.txt")
printfn "%A" (part2 <| parse "data/day22.data.txt")
