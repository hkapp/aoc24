let parse fileName =
    let s = System.IO.File.ReadAllText fileName
    s.Split(' ')
    |> Seq.map uint64

let nDigits n = (string n).Length

let evenNumberOfDigits n = ((nDigits n) % 2) = 0

let changeStone (v: uint64) =
    let splitStone n =
        let digits = string n
        let z = (nDigits n) / 2
        seq {
            digits[..(z-1)];
            digits[z..]
        }
        |> Seq.map uint64

    if v = 0UL then
        seq { 1UL }
    else if evenNumberOfDigits v then
        splitStone v
    else
        seq { 2024UL * v }

let blink stones =
    stones
    |> Seq.collect changeStone

let blinkSeq init =
    Seq.unfold
        (fun s -> Some (s, blink s))
        init

let blinkNTimes n stones =
    blinkSeq stones
    |> Seq.skip n
    |> Seq.head
    |> Seq.length

let part1 = blinkNTimes 25

let part2 = blinkNTimes 35

printfn "%A" (part1 <| parse "data/day11.test.txt")
printfn "%A" (part1 <| parse "data/day11.data.txt")
//printfn "%A" (part2 <| parse "data/day11.test.txt")
printfn "%A" (part2 <| parse "data/day11.data.txt")
