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

open System.Collections.Generic

let rec countSolutionsDP (dp: Dictionary<string, uint64>) (available: string array) s =
    let (alreadyComputed, value) = dp.TryGetValue(s)
    if alreadyComputed then
        value
    else
        let recValue =
            available
            |> Seq.choose (fun prefix ->
                if s.StartsWith(prefix) then
                    Some (s[prefix.Length..])
                else
                    None)
            |> Seq.map (countSolutionsDP dp available)
            |> Seq.sum
        dp.Add(s, recValue)
        recValue

let countSolutions (available: string array) s =
    let dp = new Dictionary<string, uint64>()
    dp.Add("", 1UL)
    countSolutionsDP dp available s

let part2 (available, desired) =
    desired
    |> Seq.map (countSolutions available)
    |> Seq.sum

printfn "%A" (parse "data/day19.test.txt")
printfn "%A" (part1 <| parse "data/day19.test.txt")
printfn "%A" (part1 <| parse "data/day19.data.txt")
printfn "%A" (part2 <| parse "data/day19.test.txt")
printfn "%A" (part2 <| parse "data/day19.data.txt")
