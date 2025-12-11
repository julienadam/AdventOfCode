#time "on"
#load "../../Tools.fs"
#load "../../Tools/MathEx.fs"
#load "../../Tools/SeqEx.fs"
#r "nuget: NFluent"
#r "nuget: FSharp.Collections.ParallelSeq"

open System
open System.Diagnostics
open System.IO
open System.Linq
open AdventOfCode
open Checked
open NFluent

type Machine = {
    numLights: int
    target: int64
    buttons: int64 list
}

let lightsToInt (input:string) =
    input.Substring(1, input.Length - 2)
    |> Seq.fold (fun state c ->
        match c with
        | '.' -> state <<< 1
        | '#' -> (state <<< 1) + 1L
        | x -> failwithf $"invalid light char {x}"
        ) 0L

Check.That(lightsToInt "[######]").IsEqualTo(0b111111)
Check.That(lightsToInt "[.#####]").IsEqualTo(0b011111)
Check.That(lightsToInt "[.##.##]").IsEqualTo(0b011011)
Check.That(lightsToInt "[......]").IsEqualTo(0)

let buttonBitMask (numLights:int) (wires:int seq)  =
    wires |> Seq.fold (fun state wire ->
        let shift = numLights - wire - 1
        state ||| (1L <<< shift)
        ) 0L

Check.That(buttonBitMask 6 [0]).IsEqualTo(0b100000)
Check.That(buttonBitMask 6 [3]).IsEqualTo(0b000100)
Check.That(buttonBitMask 6 [3;4]).IsEqualTo(0b000110)
Check.That(buttonBitMask 8 [1;2;4;6]).IsEqualTo(0b01101010)

let parseButtons n (buttons:string[]) =
    buttons
    |> Array.map (fun x ->
        x.Substring(1, x.Length - 2)
        |> splitIntList
        |> buttonBitMask n)

let parseMachine l =
    let split = ssplit " " l
    let n = split[0].Length - 2
    let wirings = split[1..split.Length-2] |> parseButtons n 
    {
      numLights = n
      target = (split[0] |> lightsToInt)
      buttons = wirings |> Seq.toList
    }

let getInput name =
    File.ReadLines(getInputPath2025 name)
    |> Seq.map parseMachine

let monkey (machine:Machine) =
    // let mutable minPresses = Int64.MaxValue
    let seen = System.Collections.Generic.Dictionary<int64, int>()
    let rec monkeyRec lights presses =
            match seen.TryGetValue(lights) with
            | true, v when v < presses  ->
                () // Cut of if state exists with fewer presses
            | _ ->
                seen[lights] <- presses // record this, as it is better that what we had
                if lights = machine.target then
                    () // Target found 
                else
                    // try pressing every button in turn
                    machine.buttons |> Seq.iter (fun b -> monkeyRec (lights ^^^ b) (presses + 1))
    monkeyRec 0L 0
    seen[machine.target]
    
Check.That(monkey (parseMachine "[.##.] (3) (1,3) (2) (2,3) (0,2) (0,1) {3,5,4,7}" )).IsEqualTo(2)
Check.That(monkey (parseMachine "[...#.] (0,2,3,4) (2,3) (0,4) (0,1,2) (1,2,3,4) {7,5,12,7,2}" )).IsEqualTo(3)
Check.That(monkey (parseMachine "[.###.#] (0,1,2,3,4) (0,3,4) (0,1,2,4,5) (1,2) {10,11,11,5,10,5}" )).IsEqualTo(2)

open FSharp.Collections.ParallelSeq
        
let solve1 input =
    getInput input
    |> PSeq.mapi (fun index machine ->
        let sw = Stopwatch.StartNew()
        let result = monkey machine
        printfn $"Solved machine {index} in {sw.Elapsed}"
        result
        )
    |> Seq.sum

Check.That(solve1 "Day10_sample1.txt").IsEqualTo(7)
// solve1 "Day10.txt"

type Machine2 = {
    numCounters: int
    expectedJoltages: int array
    buttons: int array array
}

let parseMachine2 l =
    let split = ssplit " " l
    let joltages = split[split.Length - 1]
    let n = joltages.Substring(1, joltages.Length - 2) |> splitIntList |> Seq.length
    {
        numCounters = n
        expectedJoltages = joltages.Substring(1, joltages.Length - 2) |> splitIntList
        buttons = split[1..split.Length-2] |> Array.map (fun s -> s.Substring(1,s.Length - 2) |> splitIntList)
    }
    
let getInput2 name =
    File.ReadLines(getInputPath2025 name)
    |> Seq.map parseMachine2

#r "nuget: Microsoft.Z3"
#load "../../Tools/Z3/Theory.fs"
#load "../../Tools/Z3/Bool.fs"
#load "../../Tools/Z3/Int.fs"

open Microsoft.Z3.Bool
open Microsoft.Z3.Int
open Microsoft.Z3

let solveMachine2 (machine:Machine2) =
    let rec solveMachineRec (best:bigint)=
        let buttonVariables =
            machine.buttons
            |> Seq.mapi (fun i b -> b, Int($"b{i}"))
            
        let allButtonsArePositive =
            buttonVariables
            |> Seq.map (fun (_, x) -> x >=. (bigint 0))
            |> Seq.toArray
        
        let getButtonsConnectedTo joltageIndex =
            buttonVariables
            |> Seq.map (fun (b,var) -> if b |> Array.contains joltageIndex then Some var else None)
            |> Seq.choose id
            |> Seq.toArray
          
        let buildEquation (ints:Int array) (expected:bigint) =
            (ints |> Seq.reduce (fun s1 s2 -> s1 + s2)) =. expected
          
        let sumOfPressesMustMatchCounters =
            machine.expectedJoltages
            |> Array.mapi (fun index joltage ->
                let connectedButtons = getButtonsConnectedTo index
                buildEquation connectedButtons (bigint joltage)  
            ) 
        
        let optimizeBest =
             (buttonVariables |> Seq.map snd |> Seq.reduce (fun s1 s2 -> s1 + s2)) <. best
        
        let equations = Array.concat [sumOfPressesMustMatchCounters; allButtonsArePositive; [|optimizeBest|]]
        
        match Z3.SolveResults(equations) with
        | NoSolution -> best
        | Unknown -> failwithf "no solution found"
        | Solution s -> 
            let presses =
                s |> Seq.map (fun (symbol, func, res) -> 
                    match res with 
                    | Const x -> (x :?> IntNum).Int
                    | _ -> failwithf "Not supported"
                )
                |> Seq.sum

            solveMachineRec (bigint presses)
    solveMachineRec (bigint Int64.MaxValue)

(parseMachine2 "[.##.] (3) (1,3) (2) (2,3) (0,2) (0,1) {3,5,4,7}") |> solveMachine2

let solve2 input =
    getInput2 input
    |> PSeq.mapi (fun index machine ->
        let sw = Stopwatch.StartNew()
        let result = solveMachine2 machine
        //printfn $"Solved machine {index} in {sw.Elapsed}"
        result
        )
    |> Seq.sum

Check.That(solve2 "Day10_sample1.txt").IsEqualTo(bigint 33)
solve2 "Day10.txt"
