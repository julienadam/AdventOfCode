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
    
let getInput p = 
    File.ReadAllLines(getInputPath2022 p) 
    |> Seq.map mapSensor
    |> Seq.toArray

getInput "Day15_sample1.txt" |> Dump