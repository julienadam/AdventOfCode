#time "on"
#load "../../Tools.fs"
#r "nuget: NFluent"

open System
open System.IO
open AdventOfCode
open Checked
open NFluent

let getInput name = 
    let lines = File.ReadAllLines(getInputPath2024 name)
    lines[0].Substring(12) |> int, lines[1].Substring(12) |> int, lines[2].Substring(12) |> int, lines[4].Substring(9) |> splitIntList

// First part is based on a simulator, works for all inputs
type Machine = {
    mutable regA: int64
    mutable regB: int64
    mutable regC: int64
    mutable ip: int
    mutable output: int64 list
    program: int array
}
with
    member this.combo x = 
        match x with
        | 0 | 1 | 2 | 3 -> x |> int64
        | 4 -> this.regA
        | 5 -> this.regB
        | 6 -> this.regC
        | _ -> failwithf "Invalid combo operand %i" x
    member this.adv x = 
        this.regA <- this.regA / (1L <<< (this.combo x |> int32))
        this.ip <- this.ip + 2
    member this.bxl x = 
        this.regB <- this.regB ^^^ x
        this.ip <- this.ip + 2
    member this.bst x = 
        this.regB <- (this.combo x) % 8L
        this.ip <- this.ip + 2
    member this.jnz x =
        if this.regA = 0 then
            this.ip <- this.ip + 2
        else
            this.ip <- x
    member this.bxc _ =
        this.regB <- this.regB ^^^ this.regC
        this.ip <- this.ip + 2
    member this.out x =
        this.output <- ((this.combo x) % 8L) :: this.output
        this.ip <- this.ip + 2
    member this.bdv x =
        this.regB <- this.regA / (1L <<< (this.combo x |> int32))
        this.ip <- this.ip + 2
    member this.cdv x =
        this.regC <- this.regA / (1L <<< (this.combo x |> int32))
        this.ip <- this.ip + 2
    member this.printOutput () = String.Join(",", this.output |> List.rev |> Seq.map string)
    member this.step () =
        if this.ip > this.program.Length - 2 then
            None
        else
            let opCode = this.program[this.ip]
            let f = 
                match opCode with
                | 0 -> this.adv
                | 1 -> this.bxl
                | 2 -> this.bst
                | 3 -> this.jnz
                | 4 -> this.bxc
                | 5 -> this.out
                | 6 -> this.bdv
                | 7 -> this.cdv
                | _ -> failwithf "Invalid opcode %i" opCode
            f this.program[this.ip + 1]
            Some this

let rec run (machine:Machine) = 
    match machine.step() with
    | Some m -> run m
    | None -> machine

let solve1 input =
    let a, b, c, prog = getInput input // |> Dump
    let m = run { regA = a; regB = b; regC = c; program = prog; ip = 0; output = [] }
    m.printOutput()

Check.That(solve1 "Day17_sample1.txt").Equals("4,6,3,5,6,3,5,2,1,0")

solve1 "Day17.txt"

/// Applies the program except the last instruction which loops back to the start
let applyProgramMachine m a =
    let mutable machine = { m with regA = a }
    let computationSteps = (machine.program.Length / 2) - 1
    for _ = 1 to computationSteps do
        machine <- machine.step().Value
    if machine.output.Length <> 1 then // At the end of one iteration we should have 1 result
        failwithf "Output is more than 1 elements for a single loop, should not happen"
    machine.output[0]

let solve2 input =
    let a,b,c, prog = getInput input
    let expectedList = prog |> Seq.map int64 |> Seq.toList |> List.rev
    let mutable best = Int64.MaxValue
    let template = { regA = a; regB = b; regC = c; program = prog; ip = 0; output = [] }
    let apply = applyProgramMachine template

    // Take whatever set of bits we currently have then try all combinations of 3 bits that, if added to 
    // the end, and passed into the program, outputs the expected result which is the current head of 
    // the remaining expectations, rinse and repeat until we either reach a dead end (no combination possible)
    // or a valid integer. If it's better than the one we have, keep it.
    let rec findBits (currentBits:int64) remainingExpectations =
        match remainingExpectations with
        | [] -> best <- min currentBits best
        | expected::tail ->
            [0L..7L]
            |> Seq.map (fun i -> ((currentBits <<< 3) ||| i)) // append the 3 bits of the combination to the current bits
            |> Seq.filter (fun nextBits -> (apply nextBits = expected)) // run the resulting bits in the program, keeps the ones that match
            |> Seq.iter (fun nextBits -> findBits nextBits tail) // and continue on with the rest

    // Start with 0 as the initial bits and the list of integers of the program reversed as the expectation
    findBits 0 expectedList
    best

solve2 "Day17.txt"

// Unit tests

Check.That(run { regA = 0; regB = 0; regC = 9; program = [|2;6|]; ip = 0; output = [] }).Equals({ regA = 0; regB = 1; regC = 9; program = [|2;6|]; ip = 2; output = [] })
Check.That((run { regA = 10; regB = 0; regC = 0; program = [|5;0;5;1;5;4|]; ip = 0; output = [] }).output).Equals([0;1;2])
let actual3 = run { regA = 2024; regB = 0; regC = 0; program = [|0;1;5;4;3;0|]; ip = 0; output = [] }
Check.That(actual3.printOutput()).Equals("4,2,5,6,7,7,7,7,3,1,0")
Check.That(actual3.regA).Equals(0)
let actual4 = run { regA = 0; regB = 29; regC = 0; program = [|1;7|]; ip = 0; output = [] }
Check.That(actual4.regB).Equals(26)
let actual5 = run { regA = 0; regB = 2024; regC = 43690; program = [|4;0|]; ip = 0; output = [] }
Check.That(actual5.regB).Equals(44354)

// Attempts below are specific to my input

// Find out the result of a single loop, basically applying the operations in the program up until the jump
// Here it's hardcoded but I suspect the initial simulator version would be enough and able to handle all inputs
let inline applyProgram a =
    let mutable b = a % 8L            // bst 4 - on A keep lowest 3 bits     , put in B
    b <- b ^^^ 1L                     // bxl 1 - on B flip bit 1             , put in B, odd / even thing ?
    let c = a / (1L <<< (b |> int))   // cdv 5 - on A remove last B bits     , put in C
    b <- b ^^^ 5L                     // bxl 5 - on B flip bits 1 and 3      , put in B
    b <- b ^^^ c                      // bxc 5 - on B flip C bits            , put in B
    b % 8L

// Second attempt, decode the instructions and translate to F#
// Faster but specific to the input I got
let singleRun inputA =
    let mutable a = inputA
    let mutable b = 0
    let mutable c = 0
    let output = new System.Collections.Generic.List<int>()
    while (a <> 0) do
        b <- a % 8
        b <- b ^^^ 1
        c <- a / (1 <<< b)
        b <- b ^^^ 5
        b <- b ^^^ c
        a <- a / (1 <<< 3)
        output.Add(b % 8)
    System.String.Join(',', output |> Seq.toArray)
    
Check.That(singleRun 30344604).Equals("4,3,2,6,4,5,3,2,4")