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

let run (positions: int64 array) (inputs:int64 list) =
    
    let getParam (parametersModes:ParameterMode list) index offSet =
        let mode = if offSet <= parametersModes.Length then parametersModes[offSet-1] else Position
        match mode with
        | Position -> positions[positions[index + offSet] |> int]
        | Immediate -> positions[index + offSet]

    let rec runRec index outputs remainingInputs =
        let parameterModes, opCode = decodeOpcode positions[index] 
        
        match opCode with
        | 1L ->
            let targetIdx = (positions[index + 3])
            let op1 = getParam parameterModes index 1
            let op2 = getParam parameterModes index 2
            positions[targetIdx |> int] <- op1 + op2
            runRec (index + 4) outputs remainingInputs
        | 2L -> 
            let targetIdx = (positions[index + 3])
            let op1 = getParam parameterModes index 1
            let op2 = getParam parameterModes index 2
            positions[targetIdx |> int] <- op1 * op2
            runRec (index + 4) outputs remainingInputs
        | 3L ->
            let targetPos = positions[index+1]
            positions[targetPos |> int] <- (remainingInputs |> List.head)
            runRec (index + 2) outputs (remainingInputs |> List.tail)
        | 4L ->
            let outputPos = positions[index+1]
            let output = positions[outputPos |> int]
            runRec (index + 2) (output::outputs) remainingInputs
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
            let targetIdx = (positions[index + 3])
            let op1 = getParam parameterModes index 1
            let op2 = getParam parameterModes index 2
            let value = if op1 < op2 then 1 else 0
            positions[targetIdx |> int] <- value
            runRec (index + 4) outputs remainingInputs
        | 8L -> // equals
            let targetIdx = (positions[index + 3])
            let op1 = getParam parameterModes index 1
            let op2 = getParam parameterModes index 2
            let value = if op1 = op2 then 1 else 0
            positions[targetIdx |> int] <- value
            runRec (index + 4) outputs remainingInputs
        | 99L -> 
            outputs
        | x -> failwithf $"Invalid opcode {x}" x
    runRec 0 [] inputs
 

let solve1 input =
    let acs = input |> getInput
    
    let amplify phasingSequence =
        let mutable inputSignal = 0L
        for setting in phasingSequence do
            let copy = acs |> Array.copy
            let outputs = run copy [setting;inputSignal]
            inputSignal <- outputs.Head
        inputSignal
            
    SeqEx.permutations 5 [0L;1L;2L;3L;4L]
    |> Seq.map amplify
    |> Seq.max
    
    
Check.That(solve1 "Day07_sample1.txt").IsEqualTo(43210)
Check.That(solve1 "Day07_sample2.txt").IsEqualTo(54321)
Check.That(solve1 "Day07_sample3.txt").IsEqualTo(65210)

solve1 "Day07.txt"
