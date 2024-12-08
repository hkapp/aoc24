#load "grid.fsx"

let arr2DFindOneWithPos predicate arr =
    Grid.enumerate arr
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

let arr2DGetSafe arr (x, y) =
    if Grid.withinBounds arr (x, y) then
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

let seqLenAtLeast n s =
    Seq.indexed s
    |> Seq.map fst
    |> Seq.exists ((=) n)

let loops grid startState =
    let maxSimulationLen = 4 * (Array2D.length1 grid) * (Array2D.length2 grid) + 1
    seqLenAtLeast maxSimulationLen  <| simulateGuard grid startState

let part2 grid =
    let startState = findGuard grid
    let possibleRoadBlocks =
        grid
        |> Grid.enumerate
        |> Seq.filter (fun (pos, c) -> c = '.')
        |> Seq.map fst

    possibleRoadBlocks
    |> Seq.filter (fun (i, j) ->
        // Warning! mutation
        grid[i, j] <- '#'
        let b = loops grid startState
        // Reset the mutation
        grid[i, j] <- '.'
        b
    )
    |> Seq.length

printfn "%A" (part1 <| Grid.parse "data/day6.test.txt")
printfn "%A" (part1 <| Grid.parse "data/day6.data.txt")
printfn "%A" (part2 <| Grid.parse "data/day6.test.txt")
printfn "%A" (part2 <| Grid.parse "data/day6.data.txt")
