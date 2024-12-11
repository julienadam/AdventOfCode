#time "on"
#load "../../Tools.fs"
#load "../../Tools/Digits.fs"
#r "nuget: FSharp.Collections.ParallelSeq"

open System.IO
open AdventOfCode
open FSharp.Collections.ParallelSeq
open Checked

let getInput name = File.ReadAllText(getInputPath2024 name) |> splitSpaceIntList64

type Outcome = | One of int64 | Two of int64*int64

let splitIntDigits x =
    let n = Digits.numDigits64 x
    if n % 2 = 1 then
        One (x * 2024L)
    else
        let mutable mult = 1L
        for _ = 1 to n/2 do mult <- mult * 10L
        Two (x / mult, x % mult)

splitIntDigits 123456

let blink s =
    match s with
    | 0L -> One 1L
    | x ->
        splitIntDigits x
        //let ds = digits x |> Seq.toArray
        //if ds.Length % 2 = 0 then
        //    let split = 
        //        ds
        //        |> Seq.splitInto 2
        //        |> Seq.map (fun splitDs -> splitDs |> Seq.reduce (fun a b -> a |||| b))
        //        |> Seq.toArray
        //    Two (split[0],split[1])
        //else
        //    One (x * 2024L)

let memoizedBlinkAll totalSteps initialStones =
    let memo = new System.Collections.Generic.Dictionary<(int*int64), int64>()

    let rec blink2 stone step =
        match step with
        | x when x = totalSteps -> 
            memo.TryAdd((totalSteps, stone), 1L) |> ignore
            1L
        | _ ->
            match memo.TryGetValue((step, stone)) with
            | (true, memoized) -> 
                memoized
            | (false, _) -> 
                match blink stone with
                | One s1 -> 
                    let r = blink2 s1 (step + 1)
                    memo.TryAdd((step, stone), r) |> ignore
                    r
                | Two (s1,s2) ->
                    let r = blink2 s1 (step + 1) + blink2 s2 (step + 1)
                    memo.TryAdd((step, stone), r) |> ignore
                    r
                

    let result = initialStones |> Seq.sumBy(fun s -> blink2 s 0)
    printfn "memoized a total %i intermediate results " memo.Count
    result


let solve1 = getInput >> memoizedBlinkAll 25

solve1 "Day11.txt"

let solve2 = getInput >> memoizedBlinkAll 75

solve2 "Day11.txt"
