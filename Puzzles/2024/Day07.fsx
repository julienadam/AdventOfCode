#time "on"
#load "../../Tools.fs"
#r "nuget: NFluent"

open System
open System.IO
open AdventOfCode
open NFluent

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
    |> Seq.filter (fun (exp, nums) -> couldBeTrue exp nums) // |> Seq.toArray |> Dump
    |> Seq.map fst
    |> Seq.sum

solve1 "Day07.txt"

let (||||) (a:int64) (b:int64) =
    let mutable pow = 1L
    while pow <= b do pow <- pow * 10L
    a * pow + b

Check.That(1234 |||| 567).Equals(1234567)
Check.That(1000 |||| 100).Equals(1000100)
Check.That(1 |||| 56).Equals(156)
Check.That(56 |||| 1).Equals(561)

let couldBeTrueWithConcat expected (numbers:int64 list) =
    let rec couldBeTrueWithConcatRec current remainingNumbers =
        // Skip if we're already past the target
        if current > expected then
            false
        else
            match remainingNumbers with
            | [] -> current = expected
            | h::t -> 
                if couldBeTrueWithConcatRec (current + h) t then
                    true
                else if couldBeTrueWithConcatRec (current * h) t then
                    true
                else if couldBeTrueWithConcatRec (current |||| h) t then
                    true
                else
                    false

    couldBeTrueWithConcatRec numbers.Head numbers.Tail

let solve2 input =
    getInput input
    |> Seq.filter (fun (exp, nums) -> couldBeTrueWithConcat exp nums) // |> Seq.toArray |> Dump
    |> Seq.map fst
    |> Seq.sum

solve2 "Day07.txt"