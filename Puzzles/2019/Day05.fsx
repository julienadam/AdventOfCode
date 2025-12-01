#time "on"
#load "../../Tools.fs"
#load "../../Tools/SeqEx.fs"

open System
open System.IO
open AdventOfCode
open Checked

let getInput name = File.ReadAllText(getInputPath2019 name) |> splitIntList64

let run (positions: int64 array) =
    let rec runRec index =
        // printfn "%A at index %i" positions index
        match positions[index] with
        | 1L ->
            let targetIdx = (positions[index + 3])
            let op1Idx = (positions[index + 1])
            let op2Idx = (positions[index + 2])
            positions[targetIdx |> int] <- positions[op1Idx |> int] + positions[op2Idx |> int]
            runRec (index + 4)
        | 2L -> 
            let targetIdx = (positions[index + 3])
            let op1Idx = (positions[index + 1])
            let op2Idx = (positions[index + 2])
            positions[targetIdx |> int] <- positions[op1Idx |> int] * positions[op2Idx |> int]
            runRec (index + 4)
        | 3L ->
            failwithf "TODO" 
        | 99L -> 
            ()
        | x -> failwithf "Invalid opcode %i" x
    runRec 0
    positions
    
let solve1 input =
    ()

solve1 "Day05.txt"

let solve2 input =
    ()

solve2 "Day05.txt"