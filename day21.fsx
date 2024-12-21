#load "grid.fsx"
#load "utils.fsx"
#load "sequtils.fsx"

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

(*
  y -->
 x
 |
 v
*)
let rec build (x, y) =
    if x < 0 then
        '^' :: (build (x+1, y))
    else if y > 0 then
        '>' :: (build (x, y-1))
    else if x > 0 then
        'v' :: (build (x-1, y))
    else if y < 0 then
        '<' :: (build (x, y+1))
    else
        ['A']

let convert keymap keys =
    keys
    |> Seq.append (Seq.singleton 'A')
    |> Seq.map (fun k -> Map.find k keymap)
    |> Seq.pairwise
    |> Seq.map (fun (prev, next) -> sub next prev)
    |> Seq.collect build
    |> SeqUtils.collectChars

let part1 input =
    convert keymap1 input
    |> convert keymap2
    |> convert keymap2

let validate (expected: string) (found: string) =
    if (expected.Length) = (found.Length) then
        printfn "Pass: same length"
        if expected.Equals(found) then
            printfn "Pass.2: %s" expected
        else
            printfn "Soft fail: different contents"
            printfn "  Expected: %s" expected
            printfn "  Found:    %s" found
    else
        printfn "HARD FAIL:"
        printfn "  Expected: len(%s) = %i" expected (expected.Length)
        printfn "  Found:    len(%s) = %i" found (found.Length)

let s0 = "029A"
let s1 = convert keymap1 s0
validate "<A^A>^^AvvvA" s1

let s2 = convert keymap2 s1
validate "v<<A>>^A<A>AvA<^AA>A<vAAA>^A" s2

let s3 = convert keymap2 s2
validate "<vA<AA>>^AvAA<^A>A<v<A>>^AvA^A<vA>^A<v<A>^A>AAvA^A<v<A>A>^AAAvA<^A>A" s3

validate "<v<A>>^AAAvA^A<vA<AA>>^AvAA<^A>A<v<A>A>^AAAvA<^A>A<vA>^A<A>A" (part1 "980A")
validate "<v<A>>^A<vA<A>>^AAvAA<^A>A<v<A>>^AAvA^A<vA>^AA<A>A<v<A>A>^AAAvA<^A>A" (part1 "179A")
validate "<v<A>>^AA<vA<A>>^AAvAA<^A>A<vA>^A<A>A<vA>^A<A>A<v<A>A>^AAvA<^A>A" (part1 "456A")
validate "<v<A>>^AvA^A<vA<AA>>^AAvA<^A>AAvA^A<vA>^AA<A>A<v<A>A>^AAAvA<^A>A" (part1 "379A")

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
let b1 = convert keymap1 b0
let b2 = convert keymap2 b1
validate d2 b2

let d1 = evaluate keypad2 d2
validate d1 b1

printfn "%A" (part1 "029A")
// printfn "%A" (part1 <| parse "data/day21.test.txt")
// printfn "%A" (real 2 <| parse "data/day21.data.txt")
// printfn "%A" (test 20 <| parse "data/day21.test.txt")
// printfn "%A" (real 20 <| parse "data/day21.data.txt")
