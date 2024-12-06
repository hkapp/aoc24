let parse fileName =
    System.IO.File.ReadLines fileName
    |> Seq.map Seq.toArray
    |> array2D

let arr2DIndexes arr =
    seq {
        for x in 0 .. ((Array2D.length1 arr) - 1) do
        for y in 0 .. ((Array2D.length2 arr) - 1) do
        yield (x, y)
    }

let arr2DEnumerate arr =
    arr2DIndexes arr
    |> Seq.map (fun (i, j) -> ((i, j), (Array2D.get arr i j)))

let arr2DFindOneWithPos predicate arr =
    arr2DEnumerate arr
    |> Seq.filter (fun ((i, j), x) -> predicate x)
    |> Seq.exactlyOne

let findGuard = arr2DFindOneWithPos ((=) '^')

(*  Y=0 ->
X=0
 |
 v
 *)
let moveUnchecked (x, y) dir =
    match dir with
    | '^' -> (x-1, y)
    | '>' -> (x, y+1)
    | 'v' -> (x+1, y)
    | '<' -> (x, y-1)

let arr2DWithinBounds arr (x, y) =
    x >= 0
    && x < Array2D.length1 arr
    && y >= 0
    && y < Array2D.length2 arr

let arr2DGetSafe arr (x, y) =
    if arr2DWithinBounds arr (x, y) then
        Some <| Array2D.get arr x y
    else
        None

let rotate dir =
    match dir with
    | '^' -> '>'
    | '>' -> 'v'
    | 'v' -> '<'
    | '<' -> '^'

let rec moveGuard grid currPos dir =
    let unchechedNewPos = moveUnchecked currPos dir
    match arr2DGetSafe grid unchechedNewPos with
    | None ->
        // out of the grid
        None
    | Some '#' ->
        // Path is blocked
        let newDir = rotate dir
        moveGuard grid currPos newDir
    | Some _ ->
        // Base case: simply move there
        Some (unchechedNewPos, dir)

let simulateGuard grid (startPos: int * int, startDir: char) =
    let seq1 = seq { startPos }
    let seq2 =
      Seq.unfold
          (fun (currPos, currDir) ->
              moveGuard grid currPos currDir
              |> Option.map (fun s -> (fst s, s)))
          (startPos, startDir)
    Seq.append seq1 seq2

let part1 grid =
    findGuard grid
    |> simulateGuard grid
    |> Set.ofSeq
    |> Set.count

printfn "%A" (part1 <| parse "data/day6.test.txt")
printfn "%A" (part1 <| parse "data/day6.data.txt")
// printfn "%A" (part2 <| parse "data/day4.test.txt")
// printfn "%A" (part2 <| parse "data/day4.data.txt")
