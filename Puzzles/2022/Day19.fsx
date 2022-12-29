

#time
#load "../../Tools.fsx"

open System.Text.RegularExpressions
open System.IO
open Checked
open Tools

type Blueprint = {
    id: int
    ore : int
    clay : int
    obsidian : int*int
    geode : int*int
}

let r = new Regex(@"Blueprint (?<id>\d+): Each ore robot costs (?<ore>\d+) ore\. Each clay robot costs (?<clay>\d+) ore\. Each obsidian robot costs (?<obs1>\d+) ore and (?<obs2>\d+) clay\. Each geode robot costs (?<geo1>\d+) ore and (?<geo2>\d+) obsidian\.")

let mapBlueprint line = 
    let m = r.Match line
    if not m.Success then
        failwithf "Not a valid blueprint %s" line
    else
        {
            id = m |> mInt "id"
            ore = m |> mInt "ore"
            clay = m |> mInt "clay"
            obsidian = (m |> mInt "obs1"), (m |> mInt "obs2")
            geode = (m |> mInt "geo1"), (m |> mInt "geo2")
        }

let getInput p =
    File.ReadAllLines(getInputPath2022 p)
    |> Seq.map mapBlueprint

getInput "Day19_sample1.txt"