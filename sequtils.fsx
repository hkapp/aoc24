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

let product s = Seq.fold (*) 1 s


let splitWhere predicate s =
    let idx = Seq.findIndex predicate s
    (Seq.take idx s, Seq.skip (idx+1) s)

let pairwiseNoOverlap s =
    Seq.pairwise s
    |> filteri (fun i v -> (i % 2) = 0)