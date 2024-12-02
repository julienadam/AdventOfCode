#time "on"
#load "../../Tools.fs"

open System
open System.IO
open AdventOfCode

let getInput name = 
    File.ReadAllLines(getInputPath2024 name)
    |> Array.map splitSpaceIntList64

let areLevelsSafe (levels:int64 array) =
    let s = sign(levels[1] - levels[0])
    match s with
    | 0 -> false
    | _ ->
        levels
        |> Seq.pairwise 
        |> Seq.exists(fun (a,b) -> sign(b-a) <> s || abs(b-a) < 1 || abs(b-a) > 3)
        |> not

let solve1 input =
    let levels = getInput input |> Dump
    levels |> Seq.filter areLevelsSafe
    |> Seq.length

solve1 "Day02.txt"

let areLevelsSafeWithDampener (levels:int64 array) =
   levels 
   |> Array.mapi (fun idx _ -> levels |> Array.removeAt idx)
   |> Seq.exists (fun modifiedLevel -> areLevelsSafe modifiedLevel)

let solve2 input =
    let levels = getInput input |> Dump
    let totalLevels = levels.Length

    let unsafeLevels  = 
        levels 
        |> Array.filter (areLevelsSafeWithDampener >> not)
        |> Dump

    let safeLevels = totalLevels - unsafeLevels.Length
    safeLevels

solve2 "Day02.txt"