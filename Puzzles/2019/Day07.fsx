#time "on"
#load "../../Tools.fs"
#load "../../Tools/SeqEx.fs"
#load "../../Tools/Digits.fs"

#r "nuget: NFluent"

open System
open System.IO
open NFluent
open AdventOfCode
open Checked

let getInput name =
    File.ReadAllText(getInputPath2019 name) |> splitIntList64

type ParameterMode = | Position | Immediate

let decodeOpcode code =
    if code < 100L then
        [], code
    else
        let opCode = code % 100L
        let parameterModes =
            (digitsRev (code / 100L))
            |> Seq.map (fun d -> match d with | 0L -> Position | 1L -> Immediate | x -> failwithf $"Invalid parameter mode {x}")
            |> Seq.toList
        parameterModes, opCode

type RunResult =
    | Ended of int64 list
    | Output of int*int64

let run (positions: int64 array) (inputs:int64 list) startIndex=
    
    let getParam (parametersModes:ParameterMode list) index offSet =
        let mode = if offSet <= parametersModes.Length then parametersModes[offSet-1] else Position
        match mode with
        | Position -> positions[positions[index + offSet] |> int]
        | Immediate -> positions[index + offSet]

    let rec runRec index outputs remainingInputs =
        let parameterModes, opCode = decodeOpcode positions[index] 
        
        match opCode with
        | 1L ->
            let targetIdx = positions[index + 3]
            let op1 = getParam parameterModes index 1
            let op2 = getParam parameterModes index 2
            positions[targetIdx |> int] <- op1 + op2
            runRec (index + 4) outputs remainingInputs
        | 2L -> 
            let targetIdx = positions[index + 3]
            let op1 = getParam parameterModes index 1
            let op2 = getParam parameterModes index 2
            positions[targetIdx |> int] <- op1 * op2
            runRec (index + 4) outputs remainingInputs
        | 3L ->
            if remainingInputs |> List.isEmpty then
                failwithf "Pas d'input dispo"
            else
                let targetPos = positions[index+1]
                positions[targetPos |> int] <- (remainingInputs |> List.head)
                runRec (index + 2) outputs (remainingInputs |> List.tail)
        | 4L ->
            let outputPos = positions[index+1]
            let output = positions[outputPos |> int]
            // runRec (index + 2) (output::outputs) remainingInputs
            Output ((index + 2), output)
        | 5L -> // Jump if true
            let op1 = getParam parameterModes index 1
            if op1 <> 0 then
                let op2 = getParam parameterModes index 2
                runRec (op2 |> int) outputs remainingInputs
            else
                runRec (index + 3) outputs remainingInputs
        | 6L -> // Jump if false
            let op1 = getParam parameterModes index 1
            if op1 = 0 then
                let op2 = getParam parameterModes index 2
                runRec (op2 |> int) outputs remainingInputs
            else
                runRec (index + 3) outputs remainingInputs
        | 7L -> // less than
            let targetIdx = positions[index + 3]
            let op1 = getParam parameterModes index 1
            let op2 = getParam parameterModes index 2
            let value = if op1 < op2 then 1 else 0
            positions[targetIdx |> int] <- value
            runRec (index + 4) outputs remainingInputs
        | 8L -> // equals
            let targetIdx = positions[index + 3]
            let op1 = getParam parameterModes index 1
            let op2 = getParam parameterModes index 2
            let value = if op1 = op2 then 1 else 0
            positions[targetIdx |> int] <- value
            runRec (index + 4) outputs remainingInputs
        | 99L -> 
            Ended outputs
        | x -> failwithf $"Invalid opcode {x}" x
    runRec startIndex [] inputs
 

let solve1 input =
    let acs = input |> getInput
    
    let amplify phasingSequence =
        let mutable inputSignal = 0L
        for setting in phasingSequence do
            let copy = acs |> Array.copy
            match run copy [setting;inputSignal] 0 with
            | Ended _ -> ()
            | Output (_, output) -> inputSignal <- output 
        inputSignal
            
    SeqEx.permutations 5 [0L;1L;2L;3L;4L]
    |> Seq.map amplify
    |> Seq.max
    
    
Check.That(solve1 "Day07_sample1.txt").IsEqualTo(43210)
Check.That(solve1 "Day07_sample2.txt").IsEqualTo(54321)
Check.That(solve1 "Day07_sample3.txt").IsEqualTo(65210)

solve1 "Day07.txt"

let solve2 input =
    let acs = input |> getInput
    
    let amplify amps =
        let mutable inputSignal = 0L
        let mutable ended = false
        let mutable loop = 0
        let states = [|1..5|] |> Array.map (fun _ -> acs |> Array.copy, 0)
        while not ended do
            for i, setting in amps |> Seq.indexed do
                let positions, index = states[i]
                let inputs = if loop = 0 then [setting; inputSignal] else [inputSignal] 
                match run positions inputs index with
                | Output (pos, output) ->
                    Array.set states i (positions, pos)
                    inputSignal <- output
                | Ended _ ->
                    ended <- true
            loop <- loop + 1
        inputSignal
            
    SeqEx.permutations 5 [5L;6;7;8;9]
    |> Seq.map amplify
    |> Seq.max
    
Check.That(solve2 "Day07_sample4.txt").IsEqualTo(139629729)
Check.That(solve2 "Day07_sample5.txt").IsEqualTo(18216)

solve2 "Day07.txt"