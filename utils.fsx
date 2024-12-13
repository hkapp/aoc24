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

let integers_u64 =
    Seq.unfold
        (fun x -> Some (x, x+1UL))
        0UL