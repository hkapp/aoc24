module Grid

let parse fileName =
    System.IO.File.ReadLines fileName
    |> Seq.map Seq.toArray
    |> array2D

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