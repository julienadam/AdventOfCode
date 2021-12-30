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

let rec aluInterpreterRec inputs instructions vars =
    let inline getOperandValue (vars: int[]) =
        function
        | Variable v2 -> vars.[v2 |> int]
        | Const x -> x

    let inline apply v1 operation operand vars = 
        let v1Int = v1 |> int
        let b = getOperandValue vars operand
        let a = vars.[v1Int]
        vars.[v1Int] <- operation a b
        vars

    // Special case as we need to check the operands
    let inline applyMod v1 operand (vars: int[]) = 
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
        match inst, inputs with
        | Inp v, input::inputTail -> 
            Array.set vars (v |> int) input
            aluInterpreterRec inputTail instrTail vars 
        | Add (v1, op), _ -> aluInterpreterRec inputs instrTail (vars |> apply v1 (+) op )
        | Mul (v1, op), _ -> aluInterpreterRec inputs instrTail (vars |> apply v1 (*) op )
        | Div (v1, op), _ -> aluInterpreterRec inputs instrTail (vars |> apply v1 (/) op )
        | Mod (v1, op), _ -> aluInterpreterRec inputs instrTail (vars |> applyMod v1 op )
        | Eql (v1, op), _ -> aluInterpreterRec inputs instrTail (vars |> apply v1 eql op )
        | _ -> failwithf "Invalid instruction %A" inst

let aluInterpreter inputs instructions = aluInterpreterRec inputs instructions [|0;0;0;0|]

module Tests = 

    let testMod alu =
        let modInstr = ["inp x"; "mod x 2" ] |> mapAluInstructions
        assert(alu [4] modInstr = [|0;0;0;0|])
        assert(alu [3] modInstr = [|0;1;0;0|])
    
        try
            let modOnNegA = ["inp x"; "mod x 2" ] |> mapAluInstructions
            alu [-1] modOnNegA |> ignore
            assert(false)
        with _ -> ()
    
        try
            let modOnNegB = ["inp x"; "mod x -2" ] |> mapAluInstructions
            alu [2] modOnNegB |> ignore
            assert(false)
        with _ -> ()

        try
            let modOnZeroB = ["inp x"; "mod x 0" ] |> mapAluInstructions
            alu [2] modOnZeroB |> ignore
            assert(false)
        with _ -> ()

        
    let test3Times alu = 
        
        (*
        Here is an ALU program which takes two input numbers, then sets z to 1 if the second input 
        number is three times larger than the first input number, or sets z to 0 otherwise:
        *)
        let alu3Instrs = ["inp z"; "inp x"; "mul z 3"; "eql z x" ] |> mapAluInstructions
        assert(alu [1; 3] alu3Instrs = [|0; 3; 0; 1|])
        assert(alu [1; 4] alu3Instrs = [|0; 4; 0; 0|])
        assert(alu [-2; -6] alu3Instrs = [|0; -6; 0; 1|])
    
    let testDecompBits alu = 
        (*
        Here is an ALU program which takes a non-negative integer as input, converts it into binary, 
        and stores the lowest (1's) bit in z, the second-lowest (2's) bit in y, the third-lowest (4's) 
        bit in x, and the fourth-lowest (8's) bit in w:
        *)
        let aluDecompBits = ["inp w"; "add z w"; "mod z 2"; "div w 2"; "add y w"; "mod y 2"; "div w 2"; "add x w"; "mod x 2"; "div w 2"; "mod w 2"] |> mapAluInstructions
        assert(alu [15] aluDecompBits = [|1; 1; 1; 1|])
        assert(alu [0] aluDecompBits = [|0; 0; 0; 0|])
        assert(alu [1] aluDecompBits = [|0; 0; 0; 1|])
        assert(alu [2] aluDecompBits = [|0; 0; 1; 0|])
        assert(alu [4] aluDecompBits = [|0; 1; 0; 0|])
        assert(alu [8] aluDecompBits = [|1; 0; 0; 0|])
    
Tests.testMod aluInterpreter
Tests.test3Times aluInterpreter
Tests.test3Times aluInterpreter
Tests.testDecompBits aluInterpreter
    
getInputPath "day24.txt" |> File.ReadAllLines |> mapAluInstructions

//TODO: This will probably be critical for perf, bench and optimize it ?
//let inline longTo14Digits (i:int64) = 
//    let mutable nb = 0
//    let out = [|0;0;0;0;0;0;0;0;0;0;0;0;0;0|]
    
//    let mutable n = i
//    while (n > 0L) do
//        out.[nb] <- (n % 10L) |> int
//        nb <- nb + 1
//        n <- n / 10L
    
//    out |> Array.rev |> Array.toList

//assert(longTo14Digits 123L = [0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 1; 2; 3])


let aluInstructions = getInputPath "day24.txt" |> File.ReadAllLines |> mapAluInstructions

let isValidSerialNumber input =
    try
        let result = aluInterpreter input aluInstructions
        if result.[0] <> 0 && result.[1] <> 0 && result.[2] <> 0 && result.[2] <> 0 then
            Some input
        else
            None
    with _ ->
        None

let genSerialNumbers () = seq {
    // Lots of allocations but should be fast enough
    for d1 = 9 downto 0 do
        printfn "Foo"
        for d2 = 9 downto 0 do
            for d3 = 9 downto 0 do
                for d4= 9 downto 0 do
                    for d5 = 9 downto 0 do
                        for d6 = 9 downto 0 do
                            for d7 = 9 downto 0 do  
                                for d8 = 9 downto 0 do
                                    for d9 = 9 downto 0 do
                                        for d10 = 9 downto 0 do
                                            for d11 = 9 downto 0 do
                                                for d12 = 9 downto 0 do
                                                    for d13 = 9 downto 0 do
                                                        for d14 = 9 downto 0 do
                                                            yield [d1;d2;d3;d4;d5;d6;d7;d8;d9;d10;d11;d12;d13;d14]
}

genSerialNumbers () |> Seq.last

let solve1 alu = 
    let result = genSerialNumbers () |> Seq.pick isValidSerialNumber
    printfn "%A" result

solve1 aluInterpreter


    


// TODO Transform instructions into a compiled F# expression ? Should be orders of mag faster ?