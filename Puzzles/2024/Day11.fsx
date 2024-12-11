#time "on"
#load "../../Tools.fs"
#load "../../Tools/Digits.fs"
#r "nuget: FSharp.Collections.ParallelSeq"

open System.IO
open AdventOfCode
open FSharp.Collections.ParallelSeq
open Checked

let getInput name = 
    File.ReadAllText(getInputPath2024 name) 
    |> splitSpaceIntList64

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
    let memo = new System.Collections.Concurrent.ConcurrentDictionary<(int*int64), int64>()

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
                memo.TryAdd((step, stone), r) |> ignore
                r

    let result = initialStones |> PSeq.sumBy(fun s -> blink2 s 0)
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
