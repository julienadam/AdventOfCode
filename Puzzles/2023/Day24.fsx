#time "on"
#load "../../Tools.fs"

open System
open System.IO
open AdventOfCode

let getInput name = File.ReadAllLines(getInputPath2023 name)

let solve1 input =
    getInput input |> Dump

solve1 "Day24_sample1.txt"
