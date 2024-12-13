module SeqUtils

let zipWithIndex s =
    s
    |> Seq.mapi (fun i x -> (i, x))

let filteri f s =
    s
    |> Seq.mapi (fun i x -> (f i x, x))
    |> Seq.filter fst
    |> Seq.map snd

let nonOverlappingWindows n s =
    Seq.windowed n s
    |> filteri (fun i v -> (i % n) = 0)
