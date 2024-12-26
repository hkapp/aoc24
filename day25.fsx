#load "sequtils.fsx"
#load "grid.fsx"

let rec splitAllWhere predicate seq =
    if not <| Seq.exists predicate seq then
        [seq]
    else
        match SeqUtils.splitWhere predicate seq with
        | (head, tail) -> head :: (splitAllWhere predicate tail)

let firstRow grid =
    Grid.enumerate grid
    |> Seq.filter (fun ((x, y), x) -> x = 0)
    |> Seq.map snd

let isKey (grid: char array2d) =
    firstRow grid
    |> Seq.forall ((=) '#')

let parse fileName =
    System.IO.File.ReadLines fileName
    |> splitAllWhere ((=) "")
    |> List.map array2D
    |> List.partition isKey

let fits (key, lock) =
    key
    |> Grid.enumerate
    |> Seq.forall (fun (pos, k) ->
        let l = Grid.get lock pos
        (k = '.') || (l = '.'))

let part1 (keys, locks) =
    List.allPairs keys locks
    |> List.filter fits
    |> List.length

printfn "%A" (part1 <| parse "data/day25.test.txt")
printfn "%A" (part1 <| parse "data/day25.data.txt")
//printfn "%A" (part2 <| parse "data/day25.test.txt")
//printfn "%A" (part2 <| parse "data/day25.data.txt")
