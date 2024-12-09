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
let compressBlocks blocks =
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
    |> Seq.mapi (fun i b ->
        match b with
        | Some fileId -> fileId * i
        | None -> 0)
    |> Seq.map uint64
    |> Seq.sum

let process compressionMethod fs =
    let blocks =
        expand fs
        |> Array.ofSeq
    // Note: `compress` will mutate `blocks`
    compressionMethod blocks
    checksum blocks

let part1 = process compressBlocks

let display blocks =
    for b in blocks do
        match b with
        | Some n -> printf "%i" n
        | None -> printf "."
    printfn ""

let compressFiles fileMap (blocks: 'a array) =
    let maxFileId = Map.maxKeyValue fileMap |> fst

    let findBigEnough len =
        blocks
        |> Seq.mapi (fun i x -> (i, x))
        |> Seq.scan
            (fun (seqStart, seqLen) (currIdx, block) ->
                //printf "%i:%A " currIdx block
                match block with
                | Some _ -> (currIdx+1, 0)
                | None -> (seqStart, seqLen + 1))
            (0, 0)
        |> Seq.tryFind (fun (pos, free) -> len <= free)
        |> Option.map fst

    let moveFile oldStart newStart len =
        //printfn "memcpy(tgt=%i, src=%i, len=%i)" newStart oldStart len
        for i in 0 .. (len - 1) do
            blocks[newStart + i] <- blocks[oldStart + i]
            blocks[oldStart + i] <- None

    let tryCompress fileId =
        let (fileStart, fileLen) = Map.find fileId fileMap
        let candidateNewPos = findBigEnough fileLen
        match candidateNewPos with
        | Some newPos when newPos < fileStart ->
            moveFile fileStart newPos fileLen
        | _ -> ()

    for i in maxFileId .. -1 .. 0 do
        //display blocks
        tryCompress i

let part2 fs =
    let fileMap =
        fs
        |> Seq.fold
            (fun (currPos, currMap) (inode, len) ->
                let newPos = currPos + len
                match inode with
                | Some fileId ->
                    (newPos, Map.add fileId (currPos, len) currMap)
                | None -> (newPos, currMap))
            (0, Map.empty)
        |> snd
    process (compressFiles fileMap) fs

printfn "%A" (part1 <| parse "data/day9.test.txt")
printfn "%A" (part1 <| parse "data/day9.data.txt")
printfn "%A" (part2 <| parse "data/day9.test.txt")
printfn "%A" (part2 <| parse "data/day9.data.txt")
