open System.Text.RegularExpressions

let parseCorrupted fileName =
  let regex = Regex "mul\((\d{1,3}),(\d{1,3})\)"
  System.IO.File.ReadAllText fileName
  |> regex.Matches
  |> Seq.map (fun x -> (int x.Groups[1].Value, int x.Groups[2].Value))

let part1 muls =
  muls
  |> Seq.map (fun (a, b) -> a * b)
  |> Seq.sum

printfn "%A" (part1 (parseCorrupted "data/day3.test.txt"))
printfn "%A" (part1 (parseCorrupted "data/day3.data.txt"))
