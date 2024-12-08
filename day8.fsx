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

let antennas grid =
    arr2DEnumerate grid
    |> Seq.filter (fun (pos, c) -> c <> '.')

let antennaGroups grid =
    antennas grid
    |> Seq.groupBy snd
    |> Seq.map (fun (freq, s) -> (freq, Seq.map fst s))

let allPairs arr =
    let m = (Array.length arr) - 1
    seq {
        for i in [0..m] do
        for j in [0..m] do
        if i <> j then
            yield (arr[i], arr[j])
    }

let antinodeOf ((x1, y1), (x2, y2)) =
    let f a b = 2 * a - b
    (f x1 x2, f y1 y2)

let antinodes ants =
    ants
    |> allPairs
    |> Seq.map antinodeOf

let arr2DWithinBounds arr (x, y) =
    x >= 0
    && x < Array2D.length1 arr
    && y >= 0
    && y < Array2D.length2 arr

let countTargets target grid =
    antennaGroups grid
    |> Seq.map snd
    |> Seq.map Array.ofSeq
    |> Seq.collect target
    |> Seq.filter (arr2DWithinBounds grid)
    |> Set.ofSeq
    |> Set.count

let part1 = countTargets antinodes

let allIntegers =
    Seq.unfold
        (fun x -> Some (x, x+1))
        0

let generate startPos freq =
    let f g i = (g startPos) + i * (g freq)
    allIntegers
    |> Seq.map (fun i -> (f fst i, f snd i))

let harmonics grid (u, v) =
    let g f = (f u) - (f v)
    let freq = (g fst, g snd)
    generate u freq
    |> Seq.takeWhile (arr2DWithinBounds grid)

let allHarmonics grid ants =
    ants
    |> allPairs
    |> Seq.collect (harmonics grid)

let part2 grid = countTargets (allHarmonics grid) grid

printfn "%A" (part1 <| parse "data/day8.test.txt")
printfn "%A" (part1 <| parse "data/day8.data.txt")
printfn "%A" (part2 <| parse "data/day8.test.txt")
printfn "%A" (part2 <| parse "data/day8.data.txt")
