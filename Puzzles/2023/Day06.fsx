
#time
#load "../../Tools.fs"

open System
open System.IO
open AdventOfCode
open SeqEx

type Race = {
    Duration : int
    Record : int
}

let getInput name = 
    let data = 
        File.ReadAllLines(getInputPath2023 name)
        |> Array.map (fun l -> l.Substring(9) |> splitSpaceIntList )
    Array.zip data[0] data[1]
    |> Array.map (fun (duration, distance) -> { Duration = duration; Record = distance })

let countWaysToWin race =
    [1..race.Duration - 1]
    |> Seq.filter (fun tCharge -> (race.Duration - tCharge) * tCharge > race.Record)
    |> Seq.length

let solve1 input =
    let races = getInput input
    races 
    |> Seq.map countWaysToWin 
    |> Seq.map int64
    |> SeqEx.product64

solve1 "Day06.txt"

