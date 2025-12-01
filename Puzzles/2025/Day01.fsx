#time "on"
#load "../../Tools.fs"

open System
open System.IO
open AdventOfCode
open Checked

let getInput name =
    File.ReadAllLines(getInputPath2025 name)
    |> Array.map (fun s ->
        match s[0] with
        | 'L' -> -(s.Substring(1) |> int)
        | 'R' -> s.Substring(1) |> int
        | x -> failwithf $"Not a valid rotation {x}"
        )

let solve1 input =
    getInput input
    |> Seq.scan (fun current next -> (current + next) % 100) 50
    |> Seq.filter (fun i -> i = 0)
    |> Seq.length

solve1 "Day01_sample1.txt"

solve1 "Day01.txt"

let solve2 input =
    let rotations = getInput input
    let mutable zeroes = 0
    let mutable position = 50
    for r in rotations do
        for _ in [1..(abs r)] do
            if r >= 0 then
                position <- (position + 1) % 100
            else
                position <- (position - 1) % 100
            if position = 0 then
                zeroes <- zeroes + 1
    zeroes
    
solve2 "Day01_sample1.txt"

solve2 "Day01.txt"