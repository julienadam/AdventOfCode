#time "on"
#load "../../Tools.fs"
#r "nuget: NFluent"

open System
open System.IO
open AdventOfCode
open Checked

let getInput name = 
    File.ReadAllText(getInputPath2024 name) 
    |> splitSpaceIntList64

let digitsRev (a:int64) : int64 seq = seq {
    if a = 0 then
        yield 0
    else
        let mutable a = a
        while a > 0 do 
            yield a % 10L
            a <- a / 10L
        }

let digits = digitsRev >> Seq.rev

let (||||) (a:int64) (b:int64) : int64=
    if b = 0 then a * 10L
    else
        let mutable pow = 1L
        while pow <= b do pow <- pow * 10L
        a * pow + b

let blinkSingle s =
    match s with
    | 0L -> seq { yield 1L }
    | x -> 
        let ds = digits x |> Seq.toArray
        if ds.Length % 2 = 0 then
            ds
            |> Seq.splitInto 2
            |> Seq.map (fun splitDs -> splitDs |> Seq.reduce (fun a b -> a |||| b))
        else
            seq { yield x * 2024L }

let memoizedBlinkAll initialStones totalSteps =
    let memo = new System.Collections.Generic.Dictionary<(int*int64), int64>()

    let rec blink2 stone step =
        match step with
        | x when x = totalSteps -> 
            memo.TryAdd((totalSteps, stone), 1L) |> ignore
            1L
        | x when x > totalSteps -> failwith "should not happen"
        | _ ->
            match memo.TryGetValue((step, stone)) with
            | (true, memoized) -> 
                memoized
            | (false, _) -> 
                let nextStones = blinkSingle stone
                let r = nextStones |> Seq.sumBy (fun ns -> blink2 ns (step + 1))
                memo.Add((step, stone), r)
                r

    let result = initialStones |> Seq.sumBy(fun s -> blink2 s 0)
    printfn "memoized a total %i intermediate results " memo.Count
    result


let solve1 input =
    let mutable stones = getInput input
    memoizedBlinkAll stones 25

solve1 "Day11.txt"

let solve2 input =
    let mutable stones = getInput input
    memoizedBlinkAll stones 75

solve2 "Day11.txt"
