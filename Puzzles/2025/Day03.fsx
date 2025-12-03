#time "on"
#load "../../Tools.fs"
#load "../../Tools/MathEx.fs"
#r "nuget: NFluent"
open System
open System.IO
open AdventOfCode
open Checked
open NFluent
open MathEx

let mapBank bank =
    bank |> Seq.map (fun battery -> ((int battery) - (int '0')) |> int64) |> Seq.toArray

let getInput name =
    File.ReadAllLines(getInputPath2025 name)
    |> Seq.map mapBank

let findBestJoltage length (bank:int64 array) =
    let rec findRec (remainingBatteriesInBank:int64 array) numberLeft (joltage:int64) =
        if numberLeft = 0 then
            joltage
        else
            let numCandidates = (remainingBatteriesInBank |> Array.length) - numberLeft + 1
            let indexOfBestBattery =
                remainingBatteriesInBank
                |> Seq.take numCandidates
                |> Seq.indexed
                |> Seq.maxBy snd
                |> fst
            let bestBattery = remainingBatteriesInBank[indexOfBestBattery] // |> Dump
            findRec (remainingBatteriesInBank |> Array.skip (indexOfBestBattery + 1)) (numberLeft - 1) (joltage + (pow10 (numberLeft - 1)) * bestBattery)
    findRec bank length 0 // |> Dump

let solve1 input =
    getInput input
    |> Seq.map (findBestJoltage 2)
    |> Seq.sum

Check.That(solve1 "Day03_sample1.txt").IsEqualTo(357)
solve1 "Day03.txt"

let solve2 input =
    getInput input
    |> Seq.map (findBestJoltage 12)
    |> Seq.sum

Check.That(solve2 "Day03_sample1.txt").IsEqualTo(3121910778619L)
solve2 "Day03.txt"
