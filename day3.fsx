open System.Text.RegularExpressions

type Inst =
  | Mul of int * int
  | Do
  | Dont

let parseCorrupted fileName =
  let regex = Regex "mul\((\d{1,3}),(\d{1,3})\)|do\(\)|don't\(\)"

  let createInst (m:Match) =
    match m.Groups[0].Value[0..2] with
    | "mul" -> Mul (int m.Groups[1].Value, int m.Groups[2].Value)
    | "do(" -> Do
    | "don" -> Dont

  System.IO.File.ReadAllText fileName
  |> regex.Matches
  |> Seq.map createInst

let part1 pgm =
  let applyMul inst =
    match inst with
    | Mul (a, b) -> a * b
    | _ -> 0
  pgm
  |> Seq.map applyMul
  |> Seq.sum

let part2 pgm =
  let interpret currState inst =
    match (currState, inst) with
    | ((true, currCount), Mul(a, b)) -> (true, currCount + (a * b))
    | ((false, currCount), Mul(a, b)) -> (false, currCount)
    | ((_, currCount), Do) -> (true, currCount)
    | ((_, currCount), Dont) -> (false, currCount)
  pgm
  |> Seq.fold interpret (true, 0)
  |> snd

printfn "%A" (part1 (parseCorrupted "data/day3.test.txt"))
printfn "%A" (part1 (parseCorrupted "data/day3.data.txt"))
printfn "%A" (part2 (parseCorrupted "data/day3.test2.txt"))
printfn "%A" (part2 (parseCorrupted "data/day3.data.txt"))
