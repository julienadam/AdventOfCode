#time "on"
#load "../../Tools.fs"
#load "../../Tools/Digits.fs"
#load "../../Tools/MathEx.fs"
#r "nuget: FSharp.Collections.ParallelSeq"

open System.IO
open AdventOfCode
open FSharp.Collections.ParallelSeq
open Checked

let getInput name = File.ReadAllText(getInputPath2024 name) |> splitSpaceIntList64

let memoizedBlinkAll totalSteps initialStones =
    let memo = new System.Collections.Concurrent.ConcurrentDictionary<(int*int64), int64>()

    let rec blink2 stone step =
        match step with
        | x when x = totalSteps -> 
            memo.TryAdd((totalSteps, stone), 1L) |> ignore
            1L
        | _ ->
            match memo.TryGetValue((step, stone)) with
            | (true, memoized) -> memoized
            | (false, _) -> 
                if stone = 0L then
                    let r = blink2 (1L) (step + 1)
                    memo.TryAdd((step, stone), r) |> ignore
                    r
                else
                    let n = numDigits64 stone
                    if n % 2 = 1 then
                        let r = blink2 (stone * 2024L) (step + 1)
                        memo.TryAdd((step, stone), r) |> ignore
                        r
                    else
                        let mult = MathEx.pow10 (n/2)
                        let r = blink2 (stone / mult) (step + 1) + blink2 (stone % mult) (step + 1)
                        memo.TryAdd((step, stone), r) |> ignore
                        r

    let result = initialStones |> PSeq.sumBy(fun s -> blink2 s 0)
    printfn "memoized a total %i intermediate results " memo.Count
    result


let solve1 = getInput >> memoizedBlinkAll 25

solve1 "Day11.txt"

let solve2 = getInput >> memoizedBlinkAll 75

solve2 "Day11.txt"
