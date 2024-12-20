module Grid

let parseLines lines =
    lines
    |> Seq.map Seq.toArray
    |> array2D

let parse fileName =
    System.IO.File.ReadLines fileName
    |> parseLines

let indexes arr =
    seq {
        for x in 0 .. ((Array2D.length1 arr) - 1) do
        for y in 0 .. ((Array2D.length2 arr) - 1) do
        yield (x, y)
    }

let enumerate arr =
    indexes arr
    |> Seq.map (fun (i, j) -> ((i, j), (Array2D.get arr i j)))

let withinBounds arr (x, y) =
    x >= 0
    && x < Array2D.length1 arr
    && y >= 0
    && y < Array2D.length2 arr

let get arr (x, y) = Array2D.get arr x y

let set arr (x, y) value = Array2D.set arr x y value

let neighbours (x, y) =
    seq {
        (x - 1, y);
        (x + 1, y);
        (x, y - 1);
        (x, y + 1)
    }

let validNeighbours grid pos =
    neighbours pos
    |> Seq.filter (withinBounds grid)

let getSafe arr (x, y) =
    if withinBounds arr (x, y) then
        Some <| Array2D.get arr x y
    else
        None

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

let rotateRight dir =
    match dir with
    | '^' -> '>'
    | '>' -> 'v'
    | 'v' -> '<'
    | '<' -> '^'

let rotateLeft dir =
    match dir with
    | '^' -> '<'
    | '>' -> '^'
    | 'v' -> '>'
    | '<' -> 'v'

let manhattanDistance (x1, y1) (x2, y2) =
    (abs (x1 - x2)) + (abs (y1 - y2))

let lookWithIndex grid from dir =
    Seq.unfold
        (fun pos ->
            getSafe grid pos
            |> Option.map (fun x -> ((pos, x), moveUnchecked pos dir)))
        from
    |> Seq.tail

let findTile predicate grid =
    grid
    |> enumerate
    |> Seq.filter (fun (pos, t) -> predicate t)
    |> Seq.exactlyOne
    |> fst
