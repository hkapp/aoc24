let parse fileName =
    System.IO.File.ReadLines fileName
    |> Seq.map Seq.toArray
    |> array2D

let generateIndexesStarting (startX, startY) =
    let deltas dX dY =
        [0..3]
        |> List.map (fun i -> (startX + (dX * i), startY + (dY * i)))

    seq {
        for dX in -1 .. 1 do
        for dY in -1 .. 1 do
        if not (dX = 0 && dY = 0)
        then yield deltas dX dY
    }

let validIndex arr (x, y) =
    x >= 0
    && x < Array2D.length1 arr
    && y >= 0
    && y < Array2D.length2 arr

let isXmas arr (indexes: (int * int) seq) =
    let word =
        indexes
        |> Seq.map (fun (x, y) -> Array2D.get arr x y)
        |> Seq.map string
        |> String.concat ""
    word = "XMAS"

let part1 arr =
    let startingIndices =
        seq {
            for x in 0 .. ((Array2D.length1 arr) - 1) do
            for y in 0 .. ((Array2D.length2 arr) - 1) do
            yield (x, y)
        }

    let validIndexSeq = Seq.forall (validIndex arr)

    startingIndices
    |> Seq.collect generateIndexesStarting
    |> Seq.filter validIndexSeq
    |> Seq.filter (isXmas arr)
    |> Seq.length

printfn "%A" (part1 (parse "data/day4.test.txt"))
printfn "%A" (part1 (parse "data/day4.data.txt"))
// printfn "%A" (part2 (parseCorrupted "data/day4.test2.txt"))
// printfn "%A" (part2 (parseCorrupted "data/day4.data.txt"))
