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

// type ALU = int64 -> Inst list -> int[]

type Operator = 
    | Plus
    | Multiply
    | Divide
    | Modulo
    
type Equation = 
    | Constant of int
    | Input of string
    | Operation of Equation * Operator * Equation

let opStr = function | Plus -> "+" | Multiply -> "*" | Divide -> "/" | Modulo -> "%"

let rec printEquation eq =
    match eq with
    | Constant x -> sprintf "%i" x
    | Input i -> sprintf "%s" i
    | Operation (eq1, op, eq2) ->
        sprintf "(%s %s %s)" (printEquation eq1) (opStr op) (printEquation eq2)

type Condition = 
    | AreEqual of Equation * Equation
    | AreDifferent of Equation * Equation

let printCondition = 
    function
    | AreEqual (e1,e2) -> sprintf "(%s = %s)" (printEquation e1) (printEquation e2)
    | AreDifferent (e1,e2) -> sprintf "(%s <> %s)" (printEquation e1) (printEquation e2)
    
let apply index instr ((expansion, condition):Equation[] * Condition list) =
    let getOperandValue =
        function
        | Variable v -> expansion.[v |> int]
        | Const x -> Constant x
    
    match instr with
    | Inp v ->
        let vInt = v |> int
        expansion.[vInt] <- sprintf "i%i" index |> Input
        [expansion, condition]
    | Add (v,a) ->
        let vInt = v |> int
        let aVal = getOperandValue a
        match expansion.[vInt], aVal with
        | Constant 0, _ -> // 0 + b = b
            expansion.[vInt] <- aVal
        | _, Constant 0 -> // a + 0 = a
            ()
        | Constant a, Constant b ->
            expansion.[vInt] <- Constant (a + b)
        | _ -> 
            expansion.[vInt] <- Operation (expansion.[vInt], Plus, aVal)
        [expansion, condition]
    | Mul (v,a) ->
        let vInt = v |> int
        let aVal = getOperandValue a
        if aVal = Constant 0 || expansion.[vInt] = Constant 0 then
            expansion.[vInt] <- Constant 0
        else if aVal <> Constant 1 then
            expansion.[vInt] <- Operation (expansion.[vInt], Multiply, aVal)
        [expansion, condition]
    | Mod (v,a) ->
        let vInt = v |> int
        if expansion.[vInt] = Constant 0 then // 0 mod x = 0
            [expansion, condition]
        else
            let aVal = getOperandValue a
            // Check rightside >= 0 and leftside > 0 ?
            expansion.[vInt] <- Operation (expansion.[vInt], Modulo, aVal)
            [expansion, condition]
    | Div (v,a) ->
        let vInt = v |> int
        let aVal = getOperandValue a
        if aVal = Constant 0 then
            failwithf "Division by zero"
        else if aVal <> Constant 1 then // div by one is noop
            expansion.[vInt] <- Operation (expansion.[vInt], Divide, aVal)
        [expansion, condition]
    | Eql (v,a) ->
        let vInt = v |> int
        let aVal = getOperandValue a
        // TODO: Expand the conditions by implementing a isSatisfiable cond function
        match expansion.[vInt], aVal with
        | eq1, eq2 when eq1 = eq2 -> 
            expansion.[vInt] <- Constant 1
            [expansion, condition]
        | Constant a, Constant b when a = b ->
            expansion.[vInt] <- Constant 1
            [expansion, condition]
        | Constant a, Constant b when a <> b ->
            expansion.[vInt] <- Constant 0
            [expansion, condition]
        | Constant a, Input _ when a >= 10 || a < 0 ->
            expansion.[vInt] <- Constant 0
            [expansion, condition]
        | _ ->
            let e1 = expansion |> Array.copy
            let e2 = expansion |> Array.copy
            e1.[vInt] <- Constant 1
            e1.[vInt] <- Constant 0
            [
                (e1, (AreEqual(expansion.[vInt], aVal)) :: condition); 
                (e2, (AreDifferent(expansion.[vInt], aVal)) :: condition)]

let rec expand index instructions expansions =
    match instructions with
    | [] -> expansions
    | instr::instrTail ->
        printfn "instruction : %A. Expansions : %i. Index %i" instr (expansions |> List.length) index
        let nextExpansions = expansions |> List.collect (fun e -> apply index instr e)
        let nextI = match instr with | Inp _ -> index + 1 | _ -> index
        expand nextI instrTail nextExpansions

let initialState () = [[|Constant 0; Constant 0; Constant 0; Constant 0|], []]

let aluDecompBits = ["inp w"; "add z w"; "mod z 2"; "div w 2"; "add y w"; "mod y 2"; "div w 2"; "add x w"; "mod x 2"; "div w 2"; "mod w 2"] |> mapAluInstructions
expand 1 aluDecompBits (initialState())

let day24Input = getInputPath "Day24.txt" |> File.ReadAllLines |> mapAluInstructions |> List.take 50
let expanded = expand 1 day24Input (initialState())
expanded |> Seq.iter (fun (eq, cond) -> 
    printf "w=%s; " (printEquation eq.[0])
    printf "x=%s; " (printEquation eq.[1])
    printf "y=%s; " (printEquation eq.[2])
    printf "z=%s; " (printEquation eq.[3])
    match cond with
    | [] -> ()
    | [a] -> printf "if %s" (printCondition a)
    | head::tail -> 
        printf "if %s" (printCondition head)
        tail |> Seq.iter (fun c -> printf " && %s" (printCondition c))
    printfn ""
    )
//printEquation 

//module AluInterpreter =

//    let rec aluInterpreterRec (input:int64) (divisor:int64) instructions (vars:int[]) =
//        let inline getOperandValue (vars: int[]) =
//            function
//            | Variable v2 -> vars.[v2 |> int]
//            | Const x -> x

//        let inline apply (v1:Var) operation operand vars = 
//            let v1Int = v1 |> int
//            let b = getOperandValue vars operand
//            let a = vars.[v1Int]
//            vars.[v1Int] <- operation a b
//            vars

//        // Special case as we need to check the operands
//        let inline applyMod (v1:Var) operand (vars: int[]) = 
//            let v1Int = v1 |> int
//            let a = vars.[v1Int]
//            if a < 0 then
//                failwithf "Left operand for %% op is negative: %i" a
//            let b = getOperandValue vars operand
//            if b <= 0 then
//                failwithf "Right operand for %% op is <=0 : %i" b

//            vars.[v1Int] <- a % b
//            vars

//        let inline eql a b = if a = b then 1 else 0

//        match instructions with
//        | [] -> vars
//        | inst :: instrTail ->
//            match inst with
//            | Inp v -> 
//                if divisor = 0L then
//                    failwithf "Divisor is 0, happens if we read too many inputs"
//                Array.set vars (v |> int) ((input / divisor) |> int)
//                aluInterpreterRec (input % divisor) (divisor / 10L) instrTail vars 
//            | Add (v1, op) -> aluInterpreterRec input divisor instrTail (vars |> apply v1 (+) op )
//            | Mul (v1, op) -> aluInterpreterRec input divisor instrTail (vars |> apply v1 (*) op )
//            | Div (v1, op) -> aluInterpreterRec input divisor instrTail (vars |> apply v1 (/) op )
//            | Mod (v1, op) -> aluInterpreterRec input divisor instrTail (vars |> applyMod v1 op )
//            | Eql (v1, op) -> aluInterpreterRec input divisor instrTail (vars |> apply v1 eql op )

//    let aluInterpreter input instructions = 
//        let div = if input.ToString().Length = 1 then 1L else 10L*((input.ToString().Length |> int64) - 1L)
//        aluInterpreterRec input div instructions [|0;0;0;0|]

//module Tests = 

//    let testMod alu =
//        let modInstr = ["inp x"; "mod x 2" ] |> mapAluInstructions
//        assert(alu 4L modInstr = [|0;0;0;0|])
//        assert(alu 3L modInstr = [|0;1;0;0|])
    
//        try
//            let modOnNegA = ["inp x"; "add x -1"; "mod x 2" ] |> mapAluInstructions
//            alu 0L modOnNegA |> ignore
//            assert(false)
//        with _ -> ()
    
//        try
//            let modOnNegB = ["inp x"; "mod x -2" ] |> mapAluInstructions
//            alu 2L modOnNegB |> ignore
//            assert(false)
//        with _ -> ()

//        try
//            let modOnZeroB = ["inp x"; "mod x 0" ] |> mapAluInstructions
//            alu 2L modOnZeroB |> ignore
//            assert(false)
//        with _ -> ()

        
//    let test3Times alu = 
        
//        (*
//        Here is an ALU program which takes two input numbers, then sets z to 1 if the second input 
//        number is three times larger than the first input number, or sets z to 0 otherwise:
//        *)
//        let alu3Instrs = ["inp z"; "inp x"; "mul z 3"; "eql z x" ] |> mapAluInstructions
//        assert(alu 13L alu3Instrs = [|0; 3; 0; 1|])
//        assert(alu 14L alu3Instrs = [|0; 4; 0; 0|])
//        //assert(alu [-2; -6] alu3Instrs = [|0; -6; 0; 1|])
    
//    let testDecompBits alu = 
//        (*
//        Here is an ALU program which takes a non-negative integer as input, converts it into binary, 
//        and stores the lowest (1's) bit in z, the second-lowest (2's) bit in y, the third-lowest (4's) 
//        bit in x, and the fourth-lowest (8's) bit in w:
//        *)
//        let aluDecompBits = ["inp w"; "add z w"; "mod z 2"; "div w 2"; "add y w"; "mod y 2"; "div w 2"; "add x w"; "mod x 2"; "div w 2"; "mod w 2"] |> mapAluInstructions
//        assert(alu 9L aluDecompBits = [|1; 0; 0; 1|])
//        assert(alu 0L aluDecompBits = [|0; 0; 0; 0|])
//        assert(alu 1L aluDecompBits = [|0; 0; 0; 1|])
//        assert(alu 2L aluDecompBits = [|0; 0; 1; 0|])
//        assert(alu 4L aluDecompBits = [|0; 1; 0; 0|])
//        assert(alu 8L aluDecompBits = [|1; 0; 0; 0|])
    
//Tests.testMod AluInterpreter.aluInterpreter
//Tests.test3Times AluInterpreter.aluInterpreter
//Tests.test3Times AluInterpreter.aluInterpreter
//Tests.testDecompBits AluInterpreter.aluInterpreter
    
//// Well, this isn't going to work, at all. Assuming a 4GHz CPU, 99.999.999.999.999 number to check would mean
//// So, about 10 months if the whole check could be done in a single CPU cycle, which it definitely is NOT.
//(99_999_999_999_999L / 4_000_000L) / 60L / 60L / 24L // 289 days
//// The first digit can be ignored, since it has no bearing on the output (for my input) so in fact that would be :
//(9_999_999_999_999L / 4_000_000L) / 60L / 60L / 24L // 28 days

