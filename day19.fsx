#load "sequtils.fsx"

let parseAvailable (s: string) = s.Split ", "

let parseDesired = id

let parse fileName =
    let (available, desired) =
        System.IO.File.ReadLines fileName
        |> SeqUtils.splitWhere ((=) "")
    (parseAvailable <| Seq.exactlyOne available, parseDesired desired)

let rec canSolve (available: string array) s =
    if s = "" then
        true
    else
        available
        |> Seq.choose (fun prefix ->
            if s.StartsWith(prefix) then
                Some (s[prefix.Length..])
            else
                None)
        |> Seq.exists (canSolve available)

let part1 (available, desired) =
    desired
    |> Seq.filter (canSolve available)
    |> Seq.length

printfn "%A" (parse "data/day19.test.txt")
printfn "%A" (part1 <| parse "data/day19.test.txt")
printfn "%A" (part1 <| parse "data/day19.data.txt")
//printfn "%A" (part2 7 12 <| parse "data/day19.test.txt")
//printfn "%A" (part2 71 1024 <| parse "data/day19.data.txt")
