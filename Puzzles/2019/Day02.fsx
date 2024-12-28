#time "on"
#load "../../Tools.fs"

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
        | 99L -> 
            ()
        | x -> failwithf "Invalid opcode %i" x
    runRec 0
    positions

let solve1 input =
    let positions = getInput input
    positions[1] <- 12L
    positions[2] <- 2L
    (run positions)[0]

//run [|1;9;10;3;2;3;11;0;99;30;40;50|]
//run [|1;0;0;0;99|]

solve1 "Day02.txt"

let solve2 input =
    let positionsInput = getInput input
    let noun, verb = 
        Seq.allPairs [0..99] [0..99] 
        |> Seq.find (fun (a,b) ->
            let positions = positionsInput |> Array.copy
            positions[1] <- a
            positions[2] <- b
            (run positions)[0] = 19690720
        )
    100 * noun + verb

solve2 "Day02.txt"