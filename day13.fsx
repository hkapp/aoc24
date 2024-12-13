open System.Text.RegularExpressions

let parseButton s =
    let regex = Regex "Button .: X\+(\d+), Y\+(\d+)"
    let matches = regex.Match s
    (int matches.Groups[1].Value, int matches.Groups[2].Value)

let parsePrize s =
    let regex = Regex "Prize: X=(\d+), Y=(\d+)"
    let matches = regex.Match s
    (int matches.Groups[1].Value, int matches.Groups[2].Value)

type Equation = {
    ButtonA: int * int
    ButtonB: int * int
    Prize:   int * int
}

let parseGroup arr =
    let a = parseButton <| Array.get arr 0
    let b = parseButton <| Array.get arr 1
    let p = parsePrize <| Array.get arr 2
    { ButtonA = a ; ButtonB = b ; Prize = p }

let nonOverlappingWindows n s =
    Seq.windowed n s
    |> Seq.mapi (fun i v -> (i % n, v))
    |> Seq.filter (fun (i, v) -> i = 0)
    |> Seq.map snd

let parse fileName =
    System.IO.File.ReadLines fileName
    |> Seq.filter ((<>) "")
    |> nonOverlappingWindows 3
    |> Seq.map parseGroup

let solveB eq na =
    let solveSide f =
        let rem = (f eq.Prize) - (na * (f eq.ButtonA))
        if rem % (f eq.ButtonB) = 0 then
            Some (rem / (f eq.ButtonB))
        else
            None

    match (solveSide fst, solveSide snd) with
    | (Some x, Some y) when x = y -> Some x
    | _ -> None

let solveEq eq =
    [0..100]
    |> Seq.filter (fun a -> a * (fst eq.ButtonA) <= (fst eq.Prize))
    |> Seq.choose (fun a ->
        solveB eq a
        |> Option.map (fun b -> (a, b)))

let cost (a, b) = 3 * a + b

let part1 eqs =
    eqs
    |> Seq.choose (fun eq ->
        let solutions = solveEq eq
        if Seq.isEmpty solutions then
            None
        else
            Some (Seq.minBy cost solutions))
    |> Seq.map cost
    |> Seq.sum

printfn "%A" (part1 <| parse "data/day13.test.txt")
printfn "%A" (part1 <| parse "data/day13.data.txt")
// printfn "%A" (part2 <| parse "data/day13.test.txt")
// printfn "%A" (part2 <| parse "data/day13.data.txt")
