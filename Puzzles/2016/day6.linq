<Query Kind="FSharpProgram" />

let toString : char seq -> string = Seq.map string >> String.concat ""
let getInputPath file = Path.Combine(Environment.GetFolderPath(Environment.SpecialFolder.UserProfile), @"CloudStation\Data\AdventOfCode\2016", file)
let path = getInputPath "day6.txt"

let lines = File.ReadAllLines(path)

module Puzzle1 =
    let solution() =
        [0..lines.[0].Length - 1]
        |> Seq.map (fun i -> lines |> Seq.map (fun l -> l.[i]))
        |> Seq.map (fun chars -> chars |> Seq.groupBy id)
        |> Seq.map (fun groups -> 
            groups 
            |> Seq.maxBy (fun (_, letters) -> letters |> Seq.length)
            |> fst)
        |> toString
        |> Dump

module Puzzle2 =
    let solution() =
        [0..lines.[0].Length - 1]
        |> Seq.map (fun i -> lines |> Seq.map (fun l -> l.[i]))
        |> Seq.map (fun chars -> chars |> Seq.groupBy id)
        |> Seq.map (fun groups -> 
            groups 
            |> Seq.minBy (fun (_, letters) -> letters |> Seq.length)
            |> fst)
        |> toString
        |> Dump
        
Puzzle2.solution()