let parseLine (l:string) =
  l.Split(' ')
  |> Seq.filter (fun w -> w <> "")
  |> Seq.map int

let parse fileName =
  System.IO.File.ReadLines fileName
  |> Seq.map parseLine
  |> Seq.transpose

let distance s =
  match Seq.toList s with
  | [a ; b] -> abs (a - b)

let part1 columns =
  columns
  |> Seq.map Seq.sort
  |> Seq.transpose
  |> Seq.map distance
  |> Seq.sum

let buildMap s =
  Seq.fold
    (fun (m: Map<int, int>) x ->
      match m.TryFind(x) with
      | Some(c) -> m.Add(x, c+1)
      | None    -> m.Add(x, 1)
    )
    Map.empty
    s

let cardJoin left right =
  let rightCounts = buildMap right
  let probe x = rightCounts.TryFind(x) |> Option.defaultValue 0
  Seq.map (fun x -> x * (probe x)) left

let part2 columns =
  match Seq.toList columns with
  | [left ; right] -> cardJoin left right |> Seq.sum

printfn "Part 1 (test): %A" (part1 (parse "data/day1.test.txt"))
printfn "Part 1 (real): %A" (part1 (parse "data/day1.data.txt"))

printfn "Part 2 (test): %A" (part2 (parse "data/day1.test.txt"))
printfn "Part 2 (real): %A" (part2 (parse "data/day1.data.txt"))
