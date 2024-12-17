open System.Diagnostics

#time "on"
#load "../../Tools.fs"
#r "nuget: NFluent"
#r "nuget: FSharp.Collections.ParallelSeq"

open System
open System.IO
open AdventOfCode
open Checked
open NFluent
open FSharp.Collections.ParallelSeq

let getInput name = 
    let lines = File.ReadAllLines(getInputPath2024 name)
    lines[0].Substring(12) |> int, lines[1].Substring(12) |> int, lines[2].Substring(12) |> int, lines[4].Substring(9) |> splitIntList

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

let solve2 input =
    let a, b, c, prog = getInput input // |> Dump
    let progRef = prog |> Seq.rev |> Seq.map int64 |> Seq.toList
    let counter = ref (Int32.MaxValue |> int64)
    let sw = Stopwatch.StartNew()
    use timer = new System.Threading.Timer((fun s -> 
        let percent = (counter.Value |> double) * 100.0 / (Int64.MaxValue |> double)
        printfn "%A %i %f2%%" sw.Elapsed (counter.Value) percent), (), TimeSpan.Zero, TimeSpan.FromSeconds(1))

    [0..7] |> PSeq.iter(fun off ->
        Seq.initInfinite (fun i -> i * 8 + off)
        |> Seq.iter(fun i ->
            System.Threading.Interlocked.Increment(counter) |> ignore
            let r = run { regA = i; regB = b; regC = c; program = prog; ip = 0; output = [] }
            if r.output = progRef then
                printfn ""
                printfn "%i" i
                failwithf "Solution found")
    )
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