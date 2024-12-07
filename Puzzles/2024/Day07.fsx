#time "on"
#load "../../Tools.fs"

open System
open System.IO
open AdventOfCode

let getInput name = 
    File.ReadAllLines(getInputPath2024 name)
    |> Array.map (fun l -> 
        let expected, numberList = l.Split ": " |> tupleize2
        expected |> int64, numberList |> splitSpaceIntList64 |> Array.toList
    )

let couldBeTrue expected (numbers:int64 list) =
    
    let rec couldBeTrueRec current remainingNumbers =
        match remainingNumbers with
        | [] -> current = expected
        | h::t -> 
            if couldBeTrueRec (current + h) t then
                true
            else if couldBeTrueRec (current * h) t then
                true
            else
                false

    couldBeTrueRec numbers.Head numbers.Tail

let solve1 input =
    getInput input
    |> Seq.filter (fun (exp, nums) -> couldBeTrue exp nums) |> Seq.toArray |> Dump
    |> Seq.map fst
    |> Seq.sum

solve1 "Day07.txt"
