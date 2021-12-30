#load "../../Tools.fsx"
#time "on"

open System
open System.Diagnostics
open System.Collections.Generic
open System.IO
open Tools

type Var = | W = 0 | X = 1 | Y = 2 | Z = 3

type Operand = 
    | Variable of Var
    | Const of int

type Inst =
    | Inp of Var
    | Add of Var * Operand
    | Mul of Var * Operand
    | Div of Var * Operand
    | Mod of Var * Operand
    | Eql of Var * Operand

let mapLine (l:string) = 
    let mapVar = 
        function 
        | "w" -> Var.W 
        | "x" -> Var.X 
        | "y" -> Var.Y 
        | "z" -> Var.Z 
        | c -> failwithf "Invalid variable %s" c
    let mapOperand = 
        function 
        | "w" -> Variable Var.W 
        | "x" -> Variable Var.X 
        | "y" -> Variable Var.Y 
        | "z" -> Variable Var.Z 
        | c -> Const (Int32.Parse(c))
    let inline getArguments (parts:string[]) = parts.[1] |> mapVar, parts.[2] |> mapOperand
    let split = l.Split(' ')
    match split.[0] with
    | "inp" -> Inp (split.[1] |> mapVar)
    | "add" -> split |> getArguments |> Add
    | "mul" -> split |> getArguments |> Mul
    | "div" -> split |> getArguments |> Div
    | "mod" -> split |> getArguments |> Mod
    | "eql" -> split |> getArguments |> Eql
    | ins -> failwithf "Unknown instruction %s" ins
    
let mapAluInstructions input = input |> Seq.map mapLine |> Seq.toList

type ALU = int64 -> Inst list -> int[]

module AluInterpreter =

    let rec aluInterpreterRec (input:int64) (divisor:int64) instructions (vars:int[]) =
        let inline getOperandValue (vars: int[]) =
            function
            | Variable v2 -> vars.[v2 |> int]
            | Const x -> x

        let inline apply (v1:Var) operation operand vars = 
            let v1Int = v1 |> int
            let b = getOperandValue vars operand
            let a = vars.[v1Int]
            vars.[v1Int] <- operation a b
            vars

        // Special case as we need to check the operands
        let inline applyMod (v1:Var) operand (vars: int[]) = 
            let v1Int = v1 |> int
            let a = vars.[v1Int]
            if a < 0 then
                failwithf "Left operand for %% op is negative: %i" a
            let b = getOperandValue vars operand
            if b <= 0 then
                failwithf "Right operand for %% op is <=0 : %i" b

            vars.[v1Int] <- a % b
            vars

        let inline eql a b = if a = b then 1 else 0

        match instructions with
        | [] -> vars
        | inst :: instrTail ->
            match inst with
            | Inp v -> 
                if divisor = 0L then
                    failwithf "Divisor is 0, happens if we read too many inputs"
                Array.set vars (v |> int) ((input / divisor) |> int)
                aluInterpreterRec (input % divisor) (divisor / 10L) instrTail vars 
            | Add (v1, op) -> aluInterpreterRec input divisor instrTail (vars |> apply v1 (+) op )
            | Mul (v1, op) -> aluInterpreterRec input divisor instrTail (vars |> apply v1 (*) op )
            | Div (v1, op) -> aluInterpreterRec input divisor instrTail (vars |> apply v1 (/) op )
            | Mod (v1, op) -> aluInterpreterRec input divisor instrTail (vars |> applyMod v1 op )
            | Eql (v1, op) -> aluInterpreterRec input divisor instrTail (vars |> apply v1 eql op )

    let aluInterpreter input instructions = 
        let div = if input.ToString().Length = 1 then 1L else 10L*((input.ToString().Length |> int64) - 1L)
        aluInterpreterRec input div instructions [|0;0;0;0|]

module Tests = 

    let testMod alu =
        let modInstr = ["inp x"; "mod x 2" ] |> mapAluInstructions
        assert(alu 4L modInstr = [|0;0;0;0|])
        assert(alu 3L modInstr = [|0;1;0;0|])
    
        try
            let modOnNegA = ["inp x"; "add x -1"; "mod x 2" ] |> mapAluInstructions
            alu 0L modOnNegA |> ignore
            assert(false)
        with _ -> ()
    
        try
            let modOnNegB = ["inp x"; "mod x -2" ] |> mapAluInstructions
            alu 2L modOnNegB |> ignore
            assert(false)
        with _ -> ()

        try
            let modOnZeroB = ["inp x"; "mod x 0" ] |> mapAluInstructions
            alu 2L modOnZeroB |> ignore
            assert(false)
        with _ -> ()

        
    let test3Times alu = 
        
        (*
        Here is an ALU program which takes two input numbers, then sets z to 1 if the second input 
        number is three times larger than the first input number, or sets z to 0 otherwise:
        *)
        let alu3Instrs = ["inp z"; "inp x"; "mul z 3"; "eql z x" ] |> mapAluInstructions
        assert(alu 13L alu3Instrs = [|0; 3; 0; 1|])
        assert(alu 14L alu3Instrs = [|0; 4; 0; 0|])
        //assert(alu [-2; -6] alu3Instrs = [|0; -6; 0; 1|])
    
    let testDecompBits alu = 
        (*
        Here is an ALU program which takes a non-negative integer as input, converts it into binary, 
        and stores the lowest (1's) bit in z, the second-lowest (2's) bit in y, the third-lowest (4's) 
        bit in x, and the fourth-lowest (8's) bit in w:
        *)
        let aluDecompBits = ["inp w"; "add z w"; "mod z 2"; "div w 2"; "add y w"; "mod y 2"; "div w 2"; "add x w"; "mod x 2"; "div w 2"; "mod w 2"] |> mapAluInstructions
        assert(alu 9L aluDecompBits = [|1; 0; 0; 1|])
        assert(alu 0L aluDecompBits = [|0; 0; 0; 0|])
        assert(alu 1L aluDecompBits = [|0; 0; 0; 1|])
        assert(alu 2L aluDecompBits = [|0; 0; 1; 0|])
        assert(alu 4L aluDecompBits = [|0; 1; 0; 0|])
        assert(alu 8L aluDecompBits = [|1; 0; 0; 0|])
    
Tests.testMod AluInterpreter.aluInterpreter
Tests.test3Times AluInterpreter.aluInterpreter
Tests.test3Times AluInterpreter.aluInterpreter
Tests.testDecompBits AluInterpreter.aluInterpreter
    
// Well, this isn't going to work, at all. Assuming a 4GHz CPU, 99.999.999.999.999 number to check would mean
// So, about 10 months if the whole check could be done in a single CPU cycle, which it definitely is NOT.
(99_999_999_999_999L / 4_000_000L) / 60L / 60L / 24L // 289 days
// The first digit can be ignored, since it has no bearing on the output (for my input) so in fact that would be :
(9_999_999_999_999L / 4_000_000L) / 60L / 60L / 24L // 28 days

getInputPath "day24.txt" |> File.ReadAllLines |> mapAluInstructions

let aluInstructions = getInputPath "day24.txt" |> File.ReadAllLines |> mapAluInstructions

let isValidSerialNumber (alu:ALU) input =
    try
        let result = alu input aluInstructions
        if result.[0] <> 0 && result.[1] <> 0 && result.[2] <> 0 && result.[2] <> 0 then
            Some input
        else
            None
    with _ ->
        None

let genSerialNumbers (upperBound:int64) = seq {
    let mutable i = upperBound
    while i > 0 do
        yield i
        //if i % 100000L = 0L then
        //    printfn "%i" i
        i <- i - 1L
}

genSerialNumbers 99_999_999_999_999L |> Seq.last

let solve1 alu max = 
    let result = genSerialNumbers max |> Seq.tryPick (isValidSerialNumber alu)
    printfn "%A" result

solve1 AluInterpreter.aluInterpreter 999999L

// TODO Transform instructions into a compiled F# expression ? Should be orders of mag faster ?

let i1 = 9
let i2 = 9
let i3 = 9
let i4 = 9
(((((i1 + 8) * 26 + i2 + 8) % 26) + 13 + i3 + 3) * 26 + i4 + 10) % 26
i4 + 10