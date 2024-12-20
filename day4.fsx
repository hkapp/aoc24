#load "grid.fsx"

let patternSun (startX, startY) =
    let deltas dX dY =
        [0..3]
        |> List.map (fun i -> (startX + (dX * i), startY + (dY * i)))

    seq {
        for dX in -1 .. 1 do
        for dY in -1 .. 1 do
        if not (dX = 0 && dY = 0)
        then yield deltas dX dY
    }

let accessWord arr (indexes: (int * int) seq) =
    indexes
    |> Seq.map (fun (x, y) -> Array2D.get arr x y)
    |> Seq.map string
    |> String.concat ""

let countXmas indexesPattern expectedWord arr =
    let validIndexSeq = Seq.forall (Grid.withinBounds arr)

    Grid.indexes arr
    |> Seq.collect indexesPattern
    |> Seq.filter validIndexSeq
    |> Seq.map (accessWord arr)
    |> Seq.filter ((=) expectedWord)
    |> Seq.length

let part1 = countXmas patternSun "XMAS"

let patternX (startX, startY) =
    let mkDiag = List.map (fun (dX, dY) -> (startX + dX, startY + dY))
    let diag1 = mkDiag [(-1, -1); (0, 0); (1, 1)]
    let diag2 = mkDiag [(-1, 1); (0, 0); (1, -1)]
    [
        diag1            @ diag2;
        (List.rev diag1) @ diag2;
        diag1            @ (List.rev diag2);
        (List.rev diag1) @ (List.rev diag2)
    ]

let part2 arr = countXmas patternX "MASMAS" arr

printfn "%A" (part1 <| Grid.parse "data/day4.test.txt")
printfn "%A" (part1 <| Grid.parse "data/day4.data.txt")
printfn "%A" (part2 <| Grid.parse "data/day4.test.txt")
printfn "%A" (part2 <| Grid.parse "data/day4.data.txt")
