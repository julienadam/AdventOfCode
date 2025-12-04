#time "on"
#load "../../Tools.fs"
#load "../../Tools/MathEx.fs"
#r "nuget: NFluent"
open System
open System.Diagnostics
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

let findBiggestLeftMostInRange start length (batteries:int64[]) =
    let mutable max = 0L
    let mutable index = -1
    for i = start to start + length - 1 do
        if batteries[i] > max then
            max <- batteries[i]
            index <- i
    index

let findBestJoltageV2 length (bank:int64 array) =
    let rec findRec startPos numberLeft (joltage:int64) =
        if numberLeft = 0 then
            joltage
        else
            let numCandidates = (bank |> Array.length) - startPos - numberLeft + 1
            let indexOfBestBattery = findBiggestLeftMostInRange startPos numCandidates bank
            let bestBattery = bank[indexOfBestBattery]
            findRec (indexOfBestBattery+1) (numberLeft - 1) (joltage + (pow10 (numberLeft - 1)) * bestBattery)
    findRec 0 length 0

let solve1 input =
    getInput input
    |> Seq.map (findBestJoltageV2 2)
    |> Seq.sum

Check.That(solve1 "Day03_sample1.txt").IsEqualTo(357)
solve1 "Day03.txt"

let solve2 input =
    getInput input
    |> Seq.map (findBestJoltageV2 12)
    |> Seq.sum

Check.That(solve2 "Day03_sample1.txt").IsEqualTo(3121910778619L)
let sw = Stopwatch.StartNew()
solve2 "Day03.txt"
printfn $"{sw}"