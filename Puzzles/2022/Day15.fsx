#load "../../Tools.fsx"
open System
open System.IO
open System.Text.RegularExpressions
open Checked
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
    // find the limits of the sensor ranges on the X axis, left and right
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


//getInput "Day15_sample1.txt" 
//|> solve1 10

//getInput "Day15.txt" 
//|> solve1 2000000

// Find the intersection between this horizontal line and
// each sensor area. This gives a list of segments.
let getSegmentsForLine y sensorAreas =
    sensorAreas |> Seq.choose (fun s -> 
        let sx, sy = s.sensor
        let dy = Math.Abs(sy - y)
        if dy > s.range then
            // line does not intersect sensor range
            None
        else
            // Line intersects, the segment goes 
            // from sensor x - (range - y difference)
            // to sensor x + (range - y difference)
            let dx = s.range - Math.Abs(sy - y)
            Some (sx-dx, sx+dx)
    )

let findHoles y min max segments = seq {
        // Sort segments by start coordinate
        let sortedByStart = segments |> Seq.sortBy fst
        let mutable x = min
        for (s,e) in sortedByStart do
            // x is inside segment
            if x >= s && x <= e then
                // fast forward to end of segment + 1
                x <- e + 1
            else if x < s then
                printfn "Hole found at %i %i" x y
                yield Some x
                x <- e + 1

        if x >= max then
            // Reached end of line or further
            yield None
        else
            // Edge case if hole found at end of line, before max
            printfn "Hole found after %i %i" x y
            yield Some x
    }


let solve2Segments min max sensorAreas =
    // Save the existing beacons positions in a set so that it can be checked quickly
    let beacons = 
        sensorAreas
        |> Seq.map (fun s -> s.beacon)
        |> Set.ofSeq 
    
    let (xHole,yHole) = 
        [min..max] // Scan by line
            |> Seq.collect (fun y -> 
                getSegmentsForLine y sensorAreas
                |> findHoles y min max
                |> Seq.choose id
                |> Seq.filter (fun x -> beacons.Contains (x,y) |> not)
                |> Seq.map (fun x -> x,y)
                )
            |> Seq.exactlyOne

    // Final computation
    (xHole |> int64) * 4_000_000L + (yHole |> int64)

let sw = System.Diagnostics.Stopwatch.StartNew()

//getInput "Day15_sample1.txt" 
//|> solve2Segments 0 20
//|> Dump

getInput "Day15.txt" 
|> solve2Segments 0 4_000_000
|> Dump

printfn "Elapsed %A" sw.Elapsed

// Runs in about 7 seconds on a very average laptop