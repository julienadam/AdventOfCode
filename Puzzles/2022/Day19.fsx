open System.Collections.Generic


#time
#load "../../Tools.fsx"
#r "nuget: FSharp.Collections.ParallelSeq"

open System.Text.RegularExpressions
open System.IO
open System.Threading
open FSharp.Collections.ParallelSeq
open Checked
open Tools

type Ore = int
type Clay = int
type Obsidian = int


type Blueprint = {
    id: int
    ore : Ore
    clay : Ore
    obsidian : Ore*Clay
    geode : Ore*Obsidian
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

type State = {
    ore: int
    clay: int
    obsidian: int
    geode: int
    oreBots : int
    clayBots : int
    obsidianBots : int
    geodeBots : int
} with 
    member this.Collect () = this.oreBots, this.clayBots, this.obsidianBots, this.geodeBots
    member this.Update (ore, clay, obsidian, geodes) = 
        { 
            this with 
                ore = this.ore + ore
                clay = this.clay + clay
                obsidian = this.obsidian + obsidian
                geode = this.geode + geodes
        }

let initState () = {
    ore = 0
    clay = 0
    obsidian = 0
    geode = 0
    oreBots = 1
    clayBots  = 0
    obsidianBots = 0
    geodeBots = 0
}

let pickBuildOptions (blueprint:Blueprint) (state:State) = seq {
    match blueprint.geode with
    | ore, obsidian when state.ore >= ore && state.obsidian >= obsidian ->
        yield { state with geodeBots = state.geodeBots + 1; ore = state.ore - ore; obsidian = state.obsidian - obsidian }
    | _ -> ()
    match blueprint.obsidian with
    | ore, clay when state.ore >= ore && state.clay >= clay -> 
        yield { state with obsidianBots = state.obsidianBots + 1; ore = state.ore - ore; clay = state.clay - clay }
    | _ -> ()
    match blueprint.clay with
    | ore when state.ore >= ore ->
        yield { state with clayBots = state.clayBots + 1; ore = state.ore - ore }
    | _ -> ()
    match blueprint.ore with
    | ore when state.ore >= ore ->
        yield { state with oreBots = state.oreBots + 1; ore = state.ore - ore }
    | _ -> ()
    yield state
}

// TODO : heuristics ?
let maxGeodesForBlueprint blueprint =
    let memo = new Dictionary<State*int, int>();
    let mutable counter = 0
    let mutable hitCount = 0
    let rec maxGeodesForBlueprintRec blueprint (state:State) minute =
        let collected = state.Collect()
        counter <- counter + 1
        if counter % 1000000 = 0 then
             printfn "Blueprint %i Explored %i, memo size %i, memo hit count %i" blueprint.id counter memo.Count hitCount
        match memo.TryGetValue((state, minute)) with
        | true, v -> 
            hitCount <- hitCount + 1
            v
        | _ -> 
            let result =
                if minute = 24 then
                    // Last collection !
                    state.Update(collected).geode
                else
                    let buildOptions = pickBuildOptions blueprint state
                
                    buildOptions 
                    |> Seq.map (fun buildState ->
                        maxGeodesForBlueprintRec blueprint (buildState.Update(collected)) (minute + 1)
                    )
                    |> Seq.max
            memo.Add((state, minute), result)
            result
    maxGeodesForBlueprintRec blueprint (initState()) 1


let solve1 blueprints = 
    blueprints
    |> PSeq.map (fun bp -> bp.id * (maxGeodesForBlueprint bp))
    |> Seq.sum

getInput "Day19.txt"
|> solve1

//maxGeodesForBlueprint bp1 (initState()) 1


//let optimumState =
//    match minute with
//    | 3 -> buildOptions |> Seq.find (fun s -> s.clayBots = state.clayBots + 1)
//    | 5 -> buildOptions |> Seq.find (fun s -> s.clayBots = state.clayBots + 1)
//    | 7 -> buildOptions |> Seq.find (fun s -> s.clayBots = state.clayBots + 1)
//    | 11 -> buildOptions |> Seq.find (fun s -> s.obsidianBots = state.obsidianBots + 1)
//    | 12 -> buildOptions |> Seq.find (fun s -> s.clayBots = state.clayBots + 1)
//    | 15 -> buildOptions |> Seq.find (fun s -> s.obsidianBots = state.obsidianBots + 1)
//    | 18 -> buildOptions |> Seq.find (fun s -> s.geodeBots = state.geodeBots + 1)
//    | 21 -> buildOptions |> Seq.find (fun s -> s.geodeBots = state.geodeBots + 1)
//    | _ -> state
//maxGeodesForBlueprint blueprint (optimumState.Update(collected)) (minute + 1)
