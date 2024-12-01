#time "on"
#load "../../Tools.fs"

open System
open System.IO
open AdventOfCode

let getInput name = 
    File.ReadAllLines(getInputPath2024 name)
    |> Array.map (splitSpaceIntList64 >> Tools.tupleize2)
    |> Array.unzip

let solve1 input =
    let (left, right) = getInput input // |> Dump
    let (left, right) = (left |> Seq.sort, right |> Seq.sort) //|> Dump
    (Seq.zip left right) 
    |> Seq.map (fun (a,b) -> Math.Abs(a-b))
    |> Seq.sum

solve1 "Day01.txt"

let solve2 input =
    let (left, right) = getInput input // |> Dump
    let rightMap = right |> Seq.groupBy id |> Seq.map (fun (k,v) -> k, v |> Seq.length |> int64) |> Map.ofSeq
    left 
    |> Seq.map (fun a -> 
        match rightMap.TryFind a with
        | Some v -> v * a
        | None -> 0)
    |> Seq.sum


solve2 "Day01.txt"
