#load "../../Tools.fsx"

open System
open System.IO
open Tools

let path = getInputPath "day07.txt"
//let path = getInputPath "day07_sample1.txt"

let input = (File.ReadLines(path) |> Seq.head).Split(",") |> Array.map int |> Array.toList


module Part1 =
    let max = input |> List.max
    
    let calcFuel targetHPos =
        input |> List.map (fun crabHPos -> Math.Abs(crabHPos - targetHPos)) |> List.sum
    
    let Solve() = 
        [1..max] |> List.map calcFuel |> List.min |> Dump
        ()
        
Part1.Solve()

module Part2 =
    let max = input |> List.max
    
    let calcFuelNonLinear targetHPos =
        input 
        |> List.map (fun crabHPos -> 
            let dist = Math.Abs(crabHPos - targetHPos)
            match dist with
            | 0 -> 0
            | 1 -> 1
            | d -> d * (d + 1) / 2 )
        |> List.sum
    
    let Solve() = 
        [1..max] |> List.map calcFuelNonLinear |> List.min |> Dump
        ()
        
Part2.Solve()