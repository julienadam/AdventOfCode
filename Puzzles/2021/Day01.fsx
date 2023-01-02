#load "../../Tools.fs"
open System
open System.IO
open AdventOfCode

let getInput p = File.ReadAllLines(getInputPath p) |> Seq.map Int32.Parse

module Part1 =

    let Solve path =
        getInput path 
        |> Seq.pairwise
        |> Seq.filter (fun (a, b) -> b > a)
        |> Seq.length
        |> Dump

Part1.Solve "day01.txt"

module Part2 =

    let Solve path =
            
        getInput path 
        |> Seq.windowed 3
        |> Seq.map Seq.sum
        |> Seq.pairwise
        |> Seq.filter (fun (a, b) -> b > a)
        |> Seq.length
        |> Dump

Part2.Solve "day01.txt"
