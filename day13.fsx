#load "utils.fsx"
#load "sequtils.fsx"
open System.Text.RegularExpressions

let parseButton s =
    let regex = Regex "Button .: X\+(\d+), Y\+(\d+)"
    let matches = regex.Match s
    (int64 matches.Groups[1].Value, int64 matches.Groups[2].Value)

let parsePrize s =
    let regex = Regex "Prize: X=(\d+), Y=(\d+)"
    let matches = regex.Match s
    (int64 matches.Groups[1].Value, int64 matches.Groups[2].Value)

type Equation = {
    ButtonA: int64 * int64
    ButtonB: int64 * int64
    Prize:   int64 * int64
}

let parseGroup arr =
    let a = parseButton <| Array.get arr 0
    let b = parseButton <| Array.get arr 1
    let p = parsePrize <| Array.get arr 2
    { ButtonA = a ; ButtonB = b ; Prize = p }

let parse fileName =
    System.IO.File.ReadLines fileName
    |> Seq.filter ((<>) "")
    |> SeqUtils.nonOverlappingWindows 3
    |> Seq.map parseGroup

let optDiv p q =
    if (p % q) = 0L then
        Some (p / q)
    else
        None

let solveEq eq =
    let (a1, a2) = eq.ButtonA
    let (b1, b2) = eq.ButtonB
    let (p1, p2) = eq.Prize

    optDiv
        ((a1 * p2) - (a2 * p1))
        ((a1 * b2) - (b1 * a2))
    |> Option.bind (fun b ->
        optDiv
            (p1 - (b1 * b))
            a1
        |> Option.map (fun a -> (a, b)))

let cost (a, b) = 3L * a + b

let clawMachine eqs =
    eqs
    |> Seq.choose solveEq
    |> Seq.map cost
    |> Seq.sum

let part1 = clawMachine

let part2 eqs =
    eqs
    |> Seq.map (fun eq -> { eq with Prize = Utils.bimap ((+) 10000000000000L) eq.Prize } )
    |> clawMachine

printfn "%A" (part1 <| parse "data/day13.test.txt")
printfn "%A" (part1 <| parse "data/day13.data.txt")
printfn "%A" (part2 <| parse "data/day13.data.txt")
