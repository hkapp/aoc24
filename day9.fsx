let parse fileName =
    System.IO.File.ReadAllText fileName
    |> Seq.mapi (
        fun i c ->
            let len = int c - int '0'
            if i % 2 = 0 then
                // File
                let fileId = i / 2
                (Some fileId, len)
            else
                (None, len)
    )

let expand fs =
    let blocks (value, len) = Seq.replicate len value
    fs
    |> Seq.collect blocks

// Note: `blocks` is in-out
let compress blocks =
    let tryCompress () =
        let firstFree = Array.findIndex Option.isNone blocks
        let lastUsed = Array.findIndexBack Option.isSome blocks
        if lastUsed > firstFree then
            // common case: update the block list
            blocks[firstFree] <- blocks[lastUsed]
            blocks[lastUsed] <- None
            true
        else
            // we're done
            false

    while tryCompress () do
        // nothing to do
        ()

let checksum blocks =
    blocks
    |> Seq.choose id
    |> Seq.mapi (*)
    |> Seq.map uint64
    |> Seq.sum

let part1 fs =
    let blocks =
        expand fs
        |> Array.ofSeq
    // Note: `compress` will mutate `blocks`
    compress blocks
    checksum blocks

printfn "%A" (part1 <| parse "data/day9.test.txt")
printfn "%A" (part1 <| parse "data/day9.data.txt")
// printfn "%A" (part2 <| Grid.parse "data/day9.test.txt")
// printfn "%A" (part2 <| Grid.parse "data/day9.data.txt")
