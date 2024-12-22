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

printfn "%A" (secrets 123UL |> Seq.take 11 |> List.ofSeq)

printfn "%A" (part1 <| parse "data/day22.test.txt")
printfn "%A" (part1 <| parse "data/day22.data.txt")
// printfn "%A" (test 20 <| parse "data/day22.test.txt")
// printfn "%A" (real 20 <| parse "data/day22.data.txt")
