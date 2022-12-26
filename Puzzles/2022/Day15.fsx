#load "../../Tools.fsx"
open System
open System.IO
open System.Text.RegularExpressions
open Tools

let r = Regex(@"Sensor at x=(?<sx>-?\d+), y=(?<sy>-?\d+): closest beacon is at x=(?<bx>-?\d+), y=(?<by>-?\d+)")

let mapSensor line =
    let m = r.Match(line)
    if not m.Success then
        failwithf "Could not parse line %s" line
    else
        ((mInt "sx" m), (mInt "sy" m)), ((mInt "bx" m), (mInt "by" m)) 

type SensorArea = {
    sensor : int * int
    beacon : int * int
    range : int
}

let getInput p = 
    File.ReadAllLines(getInputPath2022 p) 
    |> Seq.map mapSensor
    |> Seq.map (fun ((sx,sy),(bx,by)) -> 
        { sensor = sx, sy; beacon = bx, by; range = manhattanDistance sx sy bx by })
    |> Seq.toArray

getInput "Day15_sample1.txt" |> Dump

let isInsideSensorRange point sensor range =
    // A point is inside the coverage area if its distance is < beacon range
    manhattanDistPoints point sensor <= range

let solve1 lineY sensorAreas =
    // find the limits of the sensor range on the X axis, left and right
    let minX = sensorAreas |> Seq.map (fun s -> (s.sensor |> fst) - s.range) |> Seq.min
    let maxX = sensorAreas |> Seq.map (fun s -> (s.sensor |> fst) + s.range) |> Seq.max
    let pointsCovered = 
        [minX..maxX] 
        |> Seq.choose (fun lineX ->
            let lineP = (lineX, lineY)
            sensorAreas |> Seq.tryFind (fun s -> isInsideSensorRange lineP s.sensor s.range))
        |> Seq.length
    // Count the sensors on this line, since obviously there IS a sensor at these coords
    let beaconsOnLine =
        sensorAreas 
        |> Seq.map (fun s -> s.beacon)
        |> Seq.distinct
        |> Dump
        |> Seq.filter (fun (x,y) -> y = lineY) // Filter out beacons ON the line
        |> Seq.length

    printfn "%i points covered, %i beacons on this line" pointsCovered beaconsOnLine
    pointsCovered - beaconsOnLine


getInput "Day15_sample1.txt" 
|> solve1 10