#load "sequtils.fsx"

open System.Text.RegularExpressions

let parseRegisters lines =
    let regex = Regex "\d+"
    lines
    |> Seq.map regex.Match
    |> Seq.map (fun m -> int64 m.Groups[0].Value)
    |> Array.ofSeq

//type Operand =
//    | LitOp of int
//    | RegOp of char
//
//type Opcode =
//    | Adv | Bxl | Bst | Jnz | Bxc | Out | Bdv | Cdv
//
//let intoInst (opcode, opn) =
//    let parsedOpcode =
//        match opcode with
//        | 0 -> Adv
//        | 1 -> Bxl
//        | 2 -> Bst
//        | 3 -> Jnz
//        | 4 -> Bxc
//        |

let parseProgram (pline: string) =
    pline[9..].Split(",")
    |> Seq.map int
    |> SeqUtils.pairwiseNoOverlap
    //|> Seq.map intoInst
    |> Array.ofSeq

let parse fileName =
    let (registers, program) =
        System.IO.File.ReadLines fileName
        |> SeqUtils.splitWhere ((=) "")
    (parseRegisters registers, parseProgram <| Seq.exactlyOne program)

type CpuState =
    { Regs:   int64 array
      Instp:  int
      Stdout: int option }

let regIdx reg = (int reg) - (int 'A')

let getReg state reg =
    state.Regs[regIdx reg]

let setReg state reg value =
    let ret = Array.copy state.Regs
    ret[regIdx reg] <- value
    ret

let moveNext state =
    state.Instp + 1

let storeInst state reg value =
    { Regs   = setReg state reg value
      Instp  = moveNext state
      Stdout = None }

let combo state operand =
    match operand with
    | 0 | 1 | 2 | 3 -> int64 operand
    | 4 -> getReg state 'A'
    | 5 -> getReg state 'B'
    | 6 -> getReg state 'C'

let adv state operand =
    let numerator = getReg state 'A'
    let denominator = pown 2 (int (combo state operand))
    let res = numerator / (int64 denominator)
    storeInst state 'A' res

let literal = int64

let bxl state operand =
    let b = getReg state 'B'
    let opval = literal operand
    (b ^^^ opval) |> storeInst state 'B'

let bst state operand =
    ((combo state operand) % 8L) |> storeInst state 'B'

let jump twiceTarget =
    let tt = int twiceTarget
    if (tt % 2) = 0 then
        tt / 2
    else
        raise(new System.ArgumentException())

let jnz state operand =
    let a = getReg state 'A'
    let instp =
        if a = 0 then
            moveNext state
        else
            jump <| literal operand
    { state with
        Instp  = instp
        Stdout = None }

let bxc state operand =
    let b = getReg state 'B'
    let c = getReg state 'C'
    (b ^^^ c) |> storeInst state 'B'

let out state operand =
    let x = (combo state operand) % 8L
    { state with
        Instp  = moveNext state
        Stdout = Some (int x) }

let bdv state operand =
    let numerator = getReg state 'A'
    let denominator = pown 2 (int (combo state operand))
    let res = numerator / (int64 denominator)
    storeInst state 'B' res

let cdv state operand =
    let numerator = getReg state 'A'
    let denominator = pown 2 (int (combo state operand))
    let res = numerator / (int64 denominator)
    storeInst state 'C' res

let eval (opcode, operand) state =
    let f =
        match opcode with
        | 0 -> adv
        | 1 -> bxl
        | 2 -> bst
        | 3 -> jnz
        | 4 -> bxc
        | 5 -> out
        | 6 -> bdv
        | 7 -> cdv
    f state operand

let step program state =
    let instp = state.Instp
    if instp < Array.length program then
        // Regular case: execute
        Some <| eval program[instp] state
    else
        None

let execute program initState =
    Seq.unfold
        (fun s -> step program s |> Option.map (fun s2 -> (s2.Stdout, s2)))
        initState

let part1 (registers, program) =
    let initState = { Regs = registers ; Instp = 0 ; Stdout = None }
    execute program initState
    |> Seq.choose id
    |> Seq.map string
    |> String.concat ","

printfn "%A" (parse "data/day17.test.txt")
printfn "%A" (part1 <| parse "data/day17.test.txt")
printfn "%A" (part1 <| parse "data/day17.data.txt")
// printfn "%A" (part2 <| parse "data/day17.test.txt")
// printfn "%A" (part2 <| parse "data/day17.data.txt")
