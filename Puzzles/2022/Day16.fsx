#load "../../Tools.fsx"
open System
open System.IO
open System.Text.RegularExpressions
open Checked
open Tools

let regexValve = Regex(@"Valve (?<v>[A-Z]{2}) has flow rate=(?<rate>\d+); tunnels? leads? to valves? (?<leadsTo>.*)")

let mapValveConfiguration line = 
    let m = regexValve.Match line
    if not m.Success then
        failwithf "Could not parse line %s" line
    (m |> mStr "v"), (m |> mInt "rate"), (m |> mStr "leadsTo" |> ssplit ",")

let getInput p = 
    File.ReadAllLines(getInputPath2022 p) 
    |> Seq.map mapValveConfiguration
    |> Seq.toArray
    |> Dump

getInput "Day16_sample1.txt"