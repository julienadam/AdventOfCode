#time "on"
#load "../../Tools.fs"
#r "nuget: NFluent"

open System
open System.IO
open AdventOfCode
open NFluent

let getInput name = File.ReadAllText(getInputPath2023 name)

let hash (s:string) : int =
    s |> Seq.fold (fun v c -> (v + (c |> int)) * 17 % 256) 0 

Check.That(hash "HASH").IsEqualTo(52)

let solve1 input =
    getInput input
    |> ssplit ","
    |> Seq.sumBy hash

Check.That(solve1 "Day15_sample1.txt").IsEqualTo(1320)

solve1 "Day15.txt"
