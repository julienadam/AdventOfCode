#r "nuget: FSharp.Quotations.Evaluator"

open FSharp.Quotations.Evaluator
open FSharp.Quotations

#load "../../Tools.fsx"
#time "on"

open System
open System.IO
open Tools

type Vars = | W = 0 | X = 1 | Y = 2 | Z = 3

type Operand = | Variable of Vars | Const of int

type Inst =
    | Inp of Vars
    | Add of Vars * Operand
    | Mul of Vars * Operand
    | Div of Vars * Operand
    | Mod of Vars * Operand
    | Eql of Vars * Operand

let mapLine (l:string) = 
    let mapVar = 
        function 
        | "w" -> Vars.W 
        | "x" -> Vars.X 
        | "y" -> Vars.Y 
        | "z" -> Vars.Z 
        | c -> failwithf "Invalid variable %s" c
    let mapOperand = 
        function 
        | "w" -> Variable Vars.W 
        | "x" -> Variable Vars.X 
        | "y" -> Variable Vars.Y 
        | "z" -> Variable Vars.Z 
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

type Operator = | Plus | Multiply | Divide | Modulo
    
type Equation = 
    | Constant of int
    | Input of int
    | Operation of Equation * Operator * Equation

let opStr = function | Plus -> "+" | Multiply -> "*" | Divide -> "/" | Modulo -> "%"

let rec printEquation eq =
    match eq with
    | Constant x -> sprintf "%i" x
    | Input i -> sprintf "i%i" i
    | Operation (eq1, op, eq2) ->
        sprintf "(%s %s %s)" (printEquation eq1) (opStr op) (printEquation eq2)

type Condition = | AreEqual of Equation * Equation | AreDifferent of Equation * Equation

let printCondition = 
    function
    | AreEqual (e1,e2) -> sprintf "(%s = %s)" (printEquation e1) (printEquation e2)
    | AreDifferent (e1,e2) -> sprintf "(%s <> %s)" (printEquation e1) (printEquation e2)
    
type ConditionSolution =
    | AlwaysEqual
    | AlwaysDifferent
    | EqualIf of (Condition * int list) * (Condition * int list)

let rec getInputUsed eq used =
    match eq with
    | Input i -> used |> Set.add i
    | Constant _ -> used
    | Operation (eq1, _, eq2) ->
        Set.union (getInputUsed eq1 used) (getInputUsed eq2 used)

let rec evaluate eq (vars:int[]) =
    match eq with
    | Input i -> vars.[i]
    | Constant c -> c
    | Operation (eq1, Plus, eq2) ->
        (evaluate eq1 vars) + (evaluate eq2 vars)
    | Operation (eq1, Divide, eq2) ->
        (evaluate eq1 vars) / (evaluate eq2 vars)
    | Operation (eq1, Multiply, eq2) ->
        (evaluate eq1 vars) * (evaluate eq2 vars)
    | Operation (eq1, Modulo, eq2) ->
        (evaluate eq1 vars) % (evaluate eq2 vars)

let isPossibleDigit1 eq (index:int) =
    [0..9] |> Seq.exists (fun i ->
        let vars = [|0;0;0;0;0;0;0;0;0;0;0;0;0;0|] 
        Array.set vars index i
        let evaluation = evaluate eq vars
        evaluation >= 0 && evaluation <= 9
    )

let isPossibleDigit2 eq (index1:int) (index2:int) =
    Seq.allPairs [0..9] [0..9]
    |> Seq.exists (fun (i1,i2) ->
        let vars = [|0;0;0;0;0;0;0;0;0;0;0;0;0;0|] 
        Array.set vars index1 i1
        Array.set vars index2 i2
        let evaluation = evaluate eq vars
        evaluation >= 0 && evaluation <= 9
    )

//let isPossibleDigit3 eq (index1:int) (index2:int) (index3:int)=
//    Seq.allPairs [0..9] [0..9] 
//    |> Seq.exists (fun (i1,i2) ->
//        let vars = [|0;0;0;0;0;0;0;0;0;0;0;0;0;0|] 
//        Array.set vars index1 i1
//        Array.set vars index2 i2
//        let evaluation = evaluate eq vars
//        evaluation >= 0 && evaluation <= 9
//    )

let checkEqualityCandidates (left:Equation) (right:Equation) =
    match left, right with
    | eq1, eq2 when eq1 = eq2 -> 
        ConditionSolution.AlwaysEqual
    | Constant a, Constant b when a = b ->
        ConditionSolution.AlwaysEqual
    | Constant a, Constant b when a <> b ->
        ConditionSolution.AlwaysDifferent
    | Constant a, Input _ when a >= 10 || a < 0 ->
        ConditionSolution.AlwaysDifferent
    | eq1, Input _ ->
        let inputs = getInputUsed eq1 Set.empty |> Seq.toList

        if inputs.Length = 1 && ((isPossibleDigit1 eq1 inputs.[0]) |> not) then
            ConditionSolution.AlwaysDifferent
        else if inputs.Length = 2 && ((isPossibleDigit2 eq1 inputs.[0] inputs.[1]) |> not) then
            ConditionSolution.AlwaysDifferent
        else    
            ((AreEqual(left, right), []), (AreDifferent(left, right), [])) |> EqualIf 
    | eq1, Constant _ ->
        ((AreEqual(left, right), []), (AreDifferent(left, right), [])) |> EqualIf 


let apply index instr ((expansion, condition):Equation[] * Condition list) =
    let getOperandValue =
        function
        | Variable v -> expansion.[v |> int]
        | Const x -> Constant x
    
    match instr with
    | Inp v ->
        let vInt = v |> int
        expansion.[vInt] <- index |> Input
        [expansion, condition]
    | Add (v,a) ->
        let vInt = v |> int
        let aVal = getOperandValue a
        match expansion.[vInt], aVal with
        | Constant 0, _             -> expansion.[vInt] <- aVal
        | _, Constant 0             -> ()
        | Constant a, Constant b    -> expansion.[vInt] <- Constant (a + b)
        | _                         -> expansion.[vInt] <- Operation (expansion.[vInt], Plus, aVal)
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
        match checkEqualityCandidates expansion.[vInt] aVal with
        | AlwaysEqual ->
            expansion.[vInt] <- Constant 1
            [expansion, condition]
        | AlwaysDifferent -> 
            expansion.[vInt] <- Constant 0
            [expansion, condition]
        | EqualIf ((condEqual, valuesEqual), (condDiff, valuesDiff)) ->
            let e1 = expansion |> Array.copy
            let e2 = expansion |> Array.copy
            e1.[vInt] <- Constant 1
            e1.[vInt] <- Constant 0
            [(e1, condEqual :: condition); (e2, condDiff :: condition)]

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

let day24Input = getInputPath "Day24.txt" |> File.ReadAllLines |> mapAluInstructions //|> List.take 75
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

