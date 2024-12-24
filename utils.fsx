module Utils

let intFromChar c =
    if c >= '0' && c <= '9' then
        int c - int '0'
    else
        raise <| System.ArgumentOutOfRangeException("c", "Not a digit")

let integers =
    Seq.unfold
        (fun x -> Some (x, x+1))
        0

let bimap f (a, b) = (f a, f b)

let fst3 (a, b, c) = a

let swap (x, y) = (y, x)

let inspect x =
    printfn "%A" x
    x
