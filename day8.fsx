#load "grid.fsx"
#load "arrayutils.fsx"
#load "utils.fsx"

let antennas grid =
    Grid.enumerate grid
    |> Seq.filter (fun (pos, c) -> c <> '.')

let antennaGroups grid =
    antennas grid
    |> Seq.groupBy snd
    |> Seq.map (fun (freq, s) -> (freq, Seq.map fst s))

let allPairs arr =
    seq {
        for i in ArrayUtils.indexes arr do
        for j in ArrayUtils.indexes arr do
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

let countTargets target grid =
    antennaGroups grid
    |> Seq.map snd
    |> Seq.map Array.ofSeq
    |> Seq.collect target
    |> Seq.filter (Grid.withinBounds grid)
    |> Set.ofSeq
    |> Set.count

let part1 = countTargets antinodes

let generate startPos freq =
    let f g i = (g startPos) + i * (g freq)
    Utils.integers
    |> Seq.map (fun i -> (f fst i, f snd i))

let harmonics grid (u, v) =
    let g f = (f u) - (f v)
    let freq = (g fst, g snd)
    generate u freq
    |> Seq.takeWhile (Grid.withinBounds grid)

let allHarmonics grid ants =
    ants
    |> allPairs
    |> Seq.collect (harmonics grid)

let part2 grid = countTargets (allHarmonics grid) grid

printfn "%A" (part1 <| Grid.parse "data/day8.test.txt")
printfn "%A" (part1 <| Grid.parse "data/day8.data.txt")
printfn "%A" (part2 <| Grid.parse "data/day8.test.txt")
printfn "%A" (part2 <| Grid.parse "data/day8.data.txt")
