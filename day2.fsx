let parseLine (l:string) =
  l.Split(' ')
  |> Seq.filter (fun w -> w <> "")
  |> Seq.map int

let parse fileName =
  System.IO.File.ReadLines fileName
  |> Seq.map parseLine

let seqAll = Seq.fold (&&) true

let pairwise f r =
  Seq.windowed 2 r
  |> Seq.map (fun q -> match Seq.toList q with | [ a ; b ] -> f a b)
  |> seqAll

let allIncreasing = pairwise (<)
let allDecreasing = pairwise (>)

let smallDeltas =
  let smallEnough d = (d >= 1) && (d <= 3)
  pairwise (fun a b -> smallEnough (abs (a - b)))

let safeReport r =
  ((allIncreasing r) || (allDecreasing r)) && (smallDeltas r)

let part1 reports =
  reports
  |> Seq.filter safeReport
  |> Seq.length

let generateAlternatives r =
  [1..(Seq.length r)]
  |> Seq.map (fun i -> Seq.removeAt (i-1) r)

let dampenSafe r =
  let safe1 = safeReport r
  let safe2 =
    generateAlternatives r
    |> Seq.exists safeReport
  safe1 || safe2

let part2 reports =
  reports
  |> Seq.filter dampenSafe
  |> Seq.length

printfn "%A" (part1 (parse "data/day2.test.txt"))
printfn "%A" (part1 (parse "data/day2.data.txt"))
printfn "%A" (part2 (parse "data/day2.test.txt"))
printfn "%A" (part2 (parse "data/day2.data.txt"))
