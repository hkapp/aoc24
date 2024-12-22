#load "grid.fsx"
#load "utils.fsx"
#load "sequtils.fsx"

let parse = System.IO.File.ReadLines

// The forbidden character
let fc = ' '

let keypad1 =
    [ [| '7' ; '8' ; '9' |] ;
      [| '4' ; '5' ; '6' |] ;
      [| '1' ; '2' ; '3' |] ;
      [| fc  ; '0' ; 'A' |] ]
    |> array2D

let keypad2 =
    [ [| fc  ; '^' ; 'A' |] ;
      [| '<' ; 'v' ; '>' |] ]
    |> array2D

let mapify grid =
    Grid.enumerate grid
    |> Seq.map Utils.swap
    |> Map.ofSeq

let keymap1 = mapify keypad1

let keymap2 = mapify keypad2

let sub (x1, y1) (x2, y2) = (x1 - x2, y1 - y2)

let buildAll (mv: int * int) =
    (*
      y -->
     x
     |
     v
    *)
    let rec buildHFirst (x, y) =
        if y > 0 then
            '>' :: (buildHFirst (x, y-1))
        else if y < 0 then
            '<' :: (buildHFirst (x, y+1))
        else if x < 0 then
            '^' :: (buildHFirst (x+1, y))
        else if x > 0 then
            'v' :: (buildHFirst (x-1, y))
        else
            ['A']

    let rec buildVFirst (x, y) =
        if x < 0 then
            '^' :: (buildVFirst (x+1, y))
        else if x > 0 then
            'v' :: (buildVFirst (x-1, y))
        else if y > 0 then
            '>' :: (buildVFirst (x, y-1))
        else if y < 0 then
            '<' :: (buildVFirst (x, y+1))
        else
            ['A']

    [buildHFirst mv ; buildVFirst mv]
    |> List.map (SeqUtils.collectChars)
    |> List.distinct

let neverPanic (keypad, keymap) startPos rep =
    rep
    |> Seq.takeWhile ((<>) 'A')
    |> Seq.scan
        (fun prevPos dir ->
            Grid.moveUnchecked prevPos dir)
        startPos
    |> Seq.forall (fun p -> (Grid.get keypad p) <> fc)

let validReps (keypad, keymap) (prev, next) =
    let prevPos = Map.find prev keymap
    let nextPos = Map.find next keymap
    let mv = sub nextPos prevPos
    buildAll mv
    |> Seq.filter (neverPanic (keypad, keymap) prevPos)

let rec combineAlts (alts: 'a seq list) : 'a list seq =
    match alts with
    | [] -> Seq.singleton []
    | nowAlts :: remAlts ->
        let remComb = combineAlts remAlts
        nowAlts
        |> Seq.collect (fun a ->
            remComb
            |> Seq.map (fun comb -> a :: comb))

let keepShortestOnly sseq =
    sseq
    |> Seq.groupBy (fun (s: string) -> s.Length)
    |> Seq.minBy fst
    |> snd

let convertAll world keys =
    keys
    |> Seq.append (Seq.singleton 'A')
    |> Seq.pairwise
    |> Seq.map (validReps world)
    |> List.ofSeq
    |> combineAlts
    |> Seq.map (String.concat "")

let world1 = (keypad1, keymap1)
let world2 = (keypad2, keymap2)

let fullEncoding input =
    convertAll world1 input
    |> keepShortestOnly
    |> Seq.collect (convertAll world2)
    |> keepShortestOnly
    |> Seq.collect (convertAll world2)
    |> keepShortestOnly

let shortestEncodingLength input =
    fullEncoding input
    |> Seq.map (fun s -> s.Length)
    |> Seq.distinct
    |> Seq.exactlyOne

let numericPart (input: string) = int (input[..((input.Length)-2)])

let part1 inputCodes =
    let complexity input =
        let len = shortestEncodingLength input
        let num = numericPart input
        num * len
    inputCodes
    |> Seq.map complexity
    |> Seq.sum

let validate (expected: string) (found: string seq) =
    let foundLens = found |> Seq.map (fun s -> s.Length) |> Seq.distinct
    if Seq.exists (fun s -> expected.Equals(s)) found then
        printfn "Pass: %s" expected
    else
        printfn "Soft fail: could not find %s" expected
        if foundLens |> Seq.exists ((=) (expected.Length)) then
            printfn "Not to worry, the same length can be found"
        else
            printfn "HARD FAIL: could not find length %i in %A" (expected.Length) foundLens

    if found |> Seq.map (fun s -> s.Length) |> Seq.distinct |> Seq.length |> (<>) 1 then
        printfn "HARD FAIL: different lengths found in %A" foundLens

    //if (expected.Length) = (found.Length) then
    //    printfn "Pass: same length"
    //    if expected.Equals(found) then
    //        printfn "Pass.2: %s" expected
    //    else
    //        printfn "Soft fail: different contents"
    //        printfn "  Expected: %s" expected
    //        printfn "  Found:    %s" found
    //else
    //    printfn "HARD FAIL:"
    //    printfn "  Expected: len(%s) = %i" expected (expected.Length)
    //    printfn "  Found:    len(%s) = %i" found (found.Length)

let s0 = "029A"
let s1 = convertAll world1 s0
validate "<A^A>^^AvvvA" s1

let s2 = s1 |> Seq.collect (convertAll world2)
validate "v<<A>>^A<A>AvA<^AA>A<vAAA>^A" s2

let s3 = s2 |> Seq.collect (convertAll world2)
validate "<vA<AA>>^AvAA<^A>A<v<A>>^AvA^A<vA>^A<v<A>^A>AAvA^A<v<A>A>^AAAvA<^A>A" s3

validate "<v<A>>^AAAvA^A<vA<AA>>^AvAA<^A>A<v<A>A>^AAAvA<^A>A<vA>^A<A>A" (fullEncoding "980A")
validate "<v<A>>^A<vA<A>>^AAvAA<^A>A<v<A>>^AAvA^A<vA>^AA<A>A<v<A>A>^AAAvA<^A>A" (fullEncoding "179A")
validate "<v<A>>^AA<vA<A>>^AAvAA<^A>A<vA>^A<A>A<vA>^A<A>A<v<A>A>^AAvA<^A>A" (fullEncoding "456A")
validate "<v<A>>^AvA^A<vA<AA>>^AAvA<^A>AAvA^A<vA>^AA<A>A<v<A>A>^AAAvA<^A>A" (fullEncoding "379A")

printfn "%A" (buildAll <| sub (Map.find '<' keymap2) (Map.find 'A' keymap2))
printfn "%A" (validReps world2 ('A', '<'))

let evaluate keypad input =
    input
    |> Seq.scan
        (fun (pos, out) c ->
            match c with
            | 'A' -> (pos, Some (Grid.get keypad pos))
            | _ -> (Grid.moveUnchecked pos c, None))
        ((Grid.findTile ((=) 'A') keypad), None)
    |> Seq.map snd
    |> Seq.choose id
    |> SeqUtils.collectChars

let d2 = evaluate keypad2 "<v<A>>^AvA^A<vA<AA>>^AAvA<^A>AAvA^A<vA>^AA<A>A<v<A>A>^AAAvA<^A>A"
let b0 = "379A"
let b1 = convertAll world1 b0
let b2 = b1 |> Seq.collect (convertAll world2)
validate d2 b2

let d1 = evaluate keypad2 d2
validate d1 b1

printfn "%A" (shortestEncodingLength "029A")
printfn "%A" (part1 <| parse "data/day21.test.txt")
printfn "%A" (part1 <| parse "data/day21.data.txt")
// printfn "%A" (test 20 <| parse "data/day21.test.txt")
// printfn "%A" (real 20 <| parse "data/day21.data.txt")
