
#time
#load "../../Tools.fs"
#r "nuget: faqt"


open System
open System.IO
open AdventOfCode

let getInput name = 
    File.ReadAllLines(getInputPath2023 name)
    |> Array.map (splitSpaceIntList >> List.ofArray)

let stepsTo0 values =
    let rec stepsTo0rec values previous=
        let nextValues = 
            values
            |> Seq.pairwise
            |> Seq.map (fun (a,b) -> b - a)
            |> Seq.toList
        if nextValues |> Seq.forall (fun i -> i = 0) then
            previous
        else
            stepsTo0rec nextValues (nextValues :: previous)
    stepsTo0rec values [values |> Seq.toList]

let inline computeNext (steps: int list list) =
    steps |> Seq.map (fun s -> s |> List.last) |> Seq.sum

let solve1 input =
    getInput input 
    |> Seq.map stepsTo0
    |> Seq.map computeNext
    |> Seq.sum

open Faqt


(stepsTo0 [0;3;6;9;12;15]).Should().SequenceEqual([[3;3;3;3;3]; [0;3;6;9;12;15]])
(stepsTo0 [1;3;6;10;15;21]).Should().SequenceEqual([[1;1;1;1]; [2;3;4;5;6]; [1;3;6;10;15;21]])
([0;3;6;9;12;15] |> stepsTo0 |> computeNext).Should().Be(18)
([1;3;6;10;15;21] |> stepsTo0 |> computeNext).Should().Be(28)
([10;13;16;21;30;45] |> stepsTo0 |> computeNext).Should().Be(68)

solve1 "Day09.txt"