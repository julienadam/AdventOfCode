#time "on"
#load "../../Tools.fs"
#r "nuget: NFluent"

open System
open System.IO
open AdventOfCode

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

let blink stones =
    stones |> Seq.collect (fun s ->
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
        )

let solve1 input =
    let mutable stones = getInput input // |> Dump
    for x = 1 to 25 do
        // printfn "Blink #%i" x
        stones <- (blink stones) |> Seq.toArray // |> Dump

    stones |> Seq.length

solve1 "Day11.txt"
