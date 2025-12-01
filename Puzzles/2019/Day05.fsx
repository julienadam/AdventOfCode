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

let getInput name = File.ReadAllText(getInputPath2019 name) |> splitIntList64

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

let run (positions: int64 array) (input:int64) =
    
    let getParam (parametersModes:ParameterMode list) index offSet =
        let mode = if offSet <= parametersModes.Length then parametersModes[offSet-1] else Position
        match mode with
        | Position -> positions[positions[index + offSet] |> int]
        | Immediate -> positions[index + offSet]

    let rec runRec index outputs =
        let parameterModes, opCode = decodeOpcode positions[index] 
        //printfn "%A at index %i modes %A opcode %i" positions index parameterModes opCode
        
        match opCode with
        | 1L ->
            let targetIdx = (positions[index + 3])
            let op1 = getParam parameterModes index 1
            let op2 = getParam parameterModes index 2
            positions[targetIdx |> int] <- op1 + op2
            runRec (index + 4) outputs
        | 2L -> 
            let targetIdx = (positions[index + 3])
            let op1 = getParam parameterModes index 1
            let op2 = getParam parameterModes index 2
            positions[targetIdx |> int] <- op1 * op2
            runRec (index + 4) outputs
        | 3L ->
            let targetPos = positions[index+1]
            positions[targetPos |> int] <- input
            runRec (index + 2) outputs
        | 4L ->
            let outputPos = positions[index+1]
            let output = positions[outputPos |> int]
            runRec (index + 2) (output::outputs)
        | 5L -> // Jump if true
            let op1 = getParam parameterModes index 1
            if op1 <> 0 then
                let op2 = getParam parameterModes index 2
                runRec (op2 |> int) outputs
            else
                runRec (index + 3) outputs
        | 6L -> // Jump if false
            let op1 = getParam parameterModes index 1
            if op1 = 0 then
                let op2 = getParam parameterModes index 2
                runRec (op2 |> int) outputs
            else
                runRec (index + 3) outputs
        | 7L -> // less than
            let targetIdx = (positions[index + 3])
            let op1 = getParam parameterModes index 1
            let op2 = getParam parameterModes index 2
            let value = if op1 < op2 then 1 else 0
            positions[targetIdx |> int] <- value
            runRec (index + 4) outputs
        | 8L -> // equals
            let targetIdx = (positions[index + 3])
            let op1 = getParam parameterModes index 1
            let op2 = getParam parameterModes index 2
            let value = if op1 = op2 then 1 else 0
            positions[targetIdx |> int] <- value
            runRec (index + 4) outputs
        | 99L -> 
            outputs
        | x -> failwithf $"Invalid opcode {x}" x
    runRec 0 []

Check.That(run [|3L;0L;4L;0L;99L|] 8).IsEqualTo([8])
    
let solve1 input =
    let positions = getInput input
    let outputs = run positions 1L
    outputs |> Seq.head

solve1 "Day05.txt"

Check.That(run [|3;9;8;9;10;9;4;9;99;-1;8|] 8).IsEqualTo([1])
Check.That(run [|3;9;8;9;10;9;4;9;99;-1;8|] 9).IsEqualTo([0])

Check.That(run [|3;9;7;9;10;9;4;9;99;-1;8|] 7).IsEqualTo([1])
Check.That(run [|3;9;7;9;10;9;4;9;99;-1;8|] 8).IsEqualTo([0])
Check.That(run [|3;9;7;9;10;9;4;9;99;-1;8|] 9).IsEqualTo([0])

Check.That(run [|3;3;1108;-1;8;3;4;3;99|] 8).IsEqualTo([1])
Check.That(run [|3;3;1108;-1;8;3;4;3;99|] 9).IsEqualTo([0])

Check.That(run [|3;3;1107;-1;8;3;4;3;99|] 7).IsEqualTo([1])
Check.That(run [|3;3;1107;-1;8;3;4;3;99|] 8).IsEqualTo([0])
Check.That(run [|3;3;1107;-1;8;3;4;3;99|] 9).IsEqualTo([0])

Check.That(run [|3;12;6;12;15;1;13;14;13;4;13;99;-1;0;1;9|] 0).IsEqualTo([0])
Check.That(run [|3;12;6;12;15;1;13;14;13;4;13;99;-1;0;1;9|] 87).IsEqualTo([1])
Check.That(run [|3;3;1105;-1;9;1101;0;0;12;4;12;99;1|] 0).IsEqualTo([0])
Check.That(run [|3;3;1105;-1;9;1101;0;0;12;4;12;99;1|] -15).IsEqualTo([1])

Check.That(run [|3;21;1008;21;8;20;1005;20;22;107;8;21;20;1006;20;31;1106;0;36;98;0;0;1002;21;125;20;4;20;1105;1;46;104;999;1105;1;46;1101;1000;1;20;4;20;1105;1;46;98;99|] 8).IsEqualTo([1000])
Check.That(run [|3;21;1008;21;8;20;1005;20;22;107;8;21;20;1006;20;31;1106;0;36;98;0;0;1002;21;125;20;4;20;1105;1;46;104;999;1105;1;46;1101;1000;1;20;4;20;1105;1;46;98;99|] 9).IsEqualTo([1001])
// Check.That(run [|3;21;1008;21;8;20;1005;20;22;107;8;21;20;1006;20;31;1106;0;36;98;0;0;1002;21;125;20;4;20;1105;1;46;104;999;1105;1;46;1101;1000;1;20;4;20;1105;1;46;98;99|] 7).IsEqualTo([999])

let solve2 input =
    let positions = getInput input
    let outputs = run positions 5L
    outputs |> Seq.head

solve2 "Day05.txt"