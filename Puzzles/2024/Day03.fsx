open System.Text.RegularExpressions

#time "on"
#load "../../Tools.fs"
#load "../../Tools/RegexTools.fs"

open System
open System.IO
open AdventOfCode

let getInput name = File.ReadAllText(getInputPath2024 name)

let reTrivial = @"mul\((?<a>\d{1,3}),(?<b>\d{1,3})\)"

let solve1 input =
    let instructions = getInput input |> Dump
    let re = new Regex(reTrivial)
    re.Matches(instructions)
    |> Dump
    |> Seq.map(fun m -> (m |> mInt64 "a") * (m |> mInt64 "b"))
    |> Seq.sum


solve1 "Day03.txt"

type Instruction =
    | Mul of int64 *int64
    | Do
    | Dont

let reDoDont = @"(mul\((?<a>\d{1,3}),(?<b>\d{1,3})\)|do\(\)|don't\(\))"

type State = {
    Sum : int64
    Enabled: bool
}

let solve2 input =
    let re = new Regex(reDoDont)
    let instructions = 
        re.Matches(getInput input)
        |> Seq.map(fun m -> 
            match m.Value with
            | "do()" -> Do
            | "don't()" -> Dont
            | _ -> Mul ((m |> mInt64 "a") , (m |> mInt64 "b")))
        |> Seq.toArray
    instructions |> Seq.fold (fun s i -> 
        match i, s.Enabled with
        | Mul (a,b), true -> {s with Sum = s.Sum + (a*b)}
        | Do, _ -> {s with Enabled = true}
        | Dont, _ -> {s with Enabled = false}
        | _ -> s
    ) { Sum = 0; Enabled = true}

solve2 "Day03.txt"