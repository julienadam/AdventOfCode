
#time
#load "../../Tools.fs"
#load "../../Tools/Ranges.fs"
#load "../../Tools/SeqEx.fs"

open System
open System.IO
open AdventOfCode
open AdventOfCode.Ranges

type Race = {
    Duration : int64
    Record : int64
}

let getInput name = 
    let data = 
        File.ReadAllLines(getInputPath2023 name)
        |> Array.map (fun l -> l.Substring(9) |> splitSpaceIntList )
    Array.zip data[0] data[1]
    |> Array.map (fun (duration, distance) -> { Duration = duration; Record = distance })

let inline winsRace race tCharge = (race.Duration - tCharge) * tCharge > race.Record

let countWaysToWin race =
    [1L..race.Duration - 1L]
    |> Seq.filter (fun tCharge -> winsRace race tCharge)
    |> Seq.length

let solve1 input =
    let races = getInput input
    races 
    |> Seq.map countWaysToWin 
    |> Seq.map int64
    |> SeqEx.product64

let getInput2 name = 
    let data = 
        File.ReadAllLines(getInputPath2023 name)
        |> Array.map (fun l -> l.Substring(9).Replace(" ", "") |> int64)
    { Duration = data[0]; Record = data[1] }


let findLimits race =
    let rec findLowerLimitRec (race:Race) (range:Range64) =
        let g = range.Middle()
        // Check g and g+1 to see if it is the boundary between loss and win
        // If not, take the lower end of the range
        // If reversed, take the upper end
        match (winsRace race g, winsRace race (g + 1L)) with
        | false, true -> g + 1L
        | false, false -> findLowerLimitRec race { Start = g; End = range.End }
        | true, _ -> findLowerLimitRec race { Start = range.Start; End = g }

    let rec findUpperLimitRec (race:Race) (range:Range64) =
        let g = range.Middle()
        // Check g and g+1 to see if it is the boundary between win and loss
        // If not, take the upper end of the range
        // If reversed, take the lower end
        match (winsRace race g, winsRace race (g + 1L)) with
        | true, false -> g
        | true, true -> findUpperLimitRec race { Start = g; End = range.End }
        | false, _  -> findUpperLimitRec race { Start = range.Start; End = g }

    let lower = findLowerLimitRec race { Start = 1L; End = race.Duration }
    let upper = findUpperLimitRec race { Start = lower; End = race.Duration }
    lower, upper

let solve2 input =
    let race = getInput2 input
    let low, high = findLimits race |> Dump
    (high - low + 1L) |> Dump

solve1 "Day06_sample.txt"
solve1 "Day06.txt"
solve2 "Day06_sample.txt"
solve2 "Day06.txt"
