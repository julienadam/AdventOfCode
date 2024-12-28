#time "on"
#load "../../Tools.fs"

open System
open System.IO
open AdventOfCode
open Checked

let getInput name = File.ReadAllLines(getInputPath2019 name)

let solve1 input =
    getInput input 
    |> Seq.map int64
    |> Seq.map (fun i -> (i / 3L) - 2L)
    |> Seq.sum

solve1 "Day01.txt"

let totalFuel mass =
    let mutable total = 0L
    let rec totalFuelRec (mass:int64) =
        let fuel = (mass / 3L) - 2L
        if fuel <= 0L then
            total
        else
            total <- total + fuel
            totalFuelRec fuel
    totalFuelRec mass

let solve2 input =
    getInput input 
    |> Seq.map int64
    |> Seq.map totalFuel
    |> Seq.sum

#r "nuget: NFluent"
open NFluent

Check.That(totalFuel 14L).Equals(2L)
Check.That(totalFuel 1969L).Equals(966L)

solve2 "Day01.txt"

