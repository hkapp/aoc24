module Utils

let intFromChar c =
    if c >= '0' && c <= '9' then
        int c - int '0'
    else
        raise <| System.ArgumentOutOfRangeException("c", "Not a digit")