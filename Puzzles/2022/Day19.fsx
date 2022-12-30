#time
#load "../../Tools.fsx"
#r "nuget: FSharp.Collections.ParallelSeq"

open System.Text.RegularExpressions
open System.IO
open System.Threading
open System.Collections.Generic
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

type Constraints = {
    maxUsableOreBots : int
    maxUsableClayBots : int
    maxUsableObsidianBots : int
}

let pickBuildOptions (blueprint:Blueprint) (state:State) (constraints:Constraints)= seq {
    // TODO : Always build a geode bot if possible (could lead to wrong solutions ?)
    match blueprint.geode with
    | ore, obsidian when state.ore >= ore && state.obsidian >= obsidian ->
        yield { state with geodeBots = state.geodeBots + 1; ore = state.ore - ore; obsidian = state.obsidian - obsidian }
    | _ ->
        let canBuildObs =
            match blueprint.obsidian with // don't build more than necessary for any bot
            | ore, clay when state.ore >= ore && state.clay >= clay && state.obsidianBots < constraints.maxUsableObsidianBots -> 
                Some { state with obsidianBots = state.obsidianBots + 1; ore = state.ore - ore; clay = state.clay - clay }
            | _ -> None
        if canBuildObs.IsSome then
            yield canBuildObs.Value

        let canBuildClay = 
            match blueprint.clay with // don't build more than necessary for any bot
            | ore when state.ore >= ore && state.clayBots < constraints.maxUsableClayBots ->
                Some { state with clayBots = state.clayBots + 1; ore = state.ore - ore }
            | _ -> None
        
        if canBuildClay.IsSome then
            yield canBuildClay.Value

        let canBuildOre =
            match blueprint.ore with // don't build more than necessary for any bot
            | ore when state.ore >= ore && state.oreBots < constraints.maxUsableOreBots ->
                Some { state with oreBots = state.oreBots + 1; ore = state.ore - ore }
            | _ -> None
        if canBuildOre.IsSome then
            yield canBuildOre.Value

        // Don't wait if all options are available
        if canBuildObs.IsNone || canBuildClay.IsNone || canBuildOre.IsNone then
            yield state
}

// TODO : more heuristics ?

let maxGeodesForBlueprint (blueprint:Blueprint) maxMinutes =
    let memo = new Dictionary<State*int, int>();
    let mutable counter = 0
    let mutable hitCount = 0
    let mutable best = 0
    let mutable cut = 0
    let geoOreCost, geoObsCost = blueprint.geode
    let obsOreCost, obsClayCost = blueprint.obsidian
    let constraints = {
        maxUsableOreBots = ([geoOreCost; obsOreCost; blueprint.clay; blueprint.ore ] |> Seq.max)
        maxUsableClayBots = obsClayCost
        maxUsableObsidianBots = geoObsCost
    }
    let printProgress _ =
        printfn "Blueprint %i Explored %i, memo size %i, memo hit count %i, best %i cut %i" blueprint.id counter memo.Count hitCount best cut
    use timer = new Timer(printProgress, null, 0, 1000)
    
    let rec maxGeodesForBlueprintRec blueprint (state:State) minute =
        let collected = state.Collect()
        counter <- counter + 1
        match memo.TryGetValue((state, minute)) with
        | true, v ->
            hitCount <- hitCount + 1
            v
        | _ -> 
            let result =
                if minute = maxMinutes then
                    // Last collection !
                    let result = state.Update(collected).geode
                    if result > best then
                        // Store best
                        best <- state.Update(collected).geode
                    result
                else
                    // Cut short if not a chance to best the leading score
                    let remainingTurns = maxMinutes - minute
                    let bestPossibleScore = 
                        state.geode + state.geodeBots * remainingTurns + ([1..remainingTurns] |> List.sum)
                    if bestPossibleScore < best then
                        cut <- cut + 1
                        0
                    else
                        let buildOptions = pickBuildOptions blueprint state constraints
                        buildOptions 
                        |> Seq.map (fun buildState ->
                            maxGeodesForBlueprintRec blueprint (buildState.Update(collected)) (minute + 1)
                        )
                        |> Seq.max
            //memo.Add((state, minute), result)
            result
    maxGeodesForBlueprintRec blueprint (initState()) 1 


let solve1 blueprints = 
    blueprints
    |> PSeq.map (fun bp -> bp.id * (maxGeodesForBlueprint bp 24))
    |> Seq.sum


getInput "Day19.txt"
|> solve1

let bp = 
    getInput "Day19_sample1.txt"
    |> Seq.head

maxGeodesForBlueprint bp 32

let solve2 blueprints =
    let result = 
        blueprints 
        |> Seq.take 3
        |> PSeq.map (fun bp -> maxGeodesForBlueprint bp 32)
        |> Seq.toArray
    result.[0] * result.[1] * result.[2]

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



//let optimumState =
//    match minute with
//    | 5 -> buildOptions |> Seq.find (fun s -> s.oreBots = state.oreBots + 1)
//    | 7 -> buildOptions |> Seq.find (fun s -> s.clayBots = state.clayBots + 1)
//    | 8 -> buildOptions |> Seq.find (fun s -> s.clayBots = state.clayBots + 1)
//    | 9 -> buildOptions |> Seq.find (fun s -> s.clayBots = state.clayBots + 1)
//    | 10 -> buildOptions |> Seq.find (fun s -> s.clayBots = state.clayBots + 1)
//    | 11 -> buildOptions |> Seq.find (fun s -> s.clayBots = state.clayBots + 1)
//    | 12 -> buildOptions |> Seq.find (fun s -> s.clayBots = state.clayBots + 1)
//    | 13 -> buildOptions |> Seq.find (fun s -> s.clayBots = state.clayBots + 1)
//    | 14 -> buildOptions |> Seq.find (fun s -> s.obsidianBots = state.obsidianBots + 1)
//    | 16 -> buildOptions |> Seq.find (fun s -> s.obsidianBots = state.obsidianBots + 1)
//    | 17 -> buildOptions |> Seq.find (fun s -> s.obsidianBots = state.obsidianBots + 1)
//    | 19 -> buildOptions |> Seq.find (fun s -> s.obsidianBots = state.obsidianBots + 1)
//    | 20 -> buildOptions |> Seq.find (fun s -> s.geodeBots = state.geodeBots + 1)
//    | 21 -> buildOptions |> Seq.find (fun s -> s.obsidianBots = state.obsidianBots + 1)
//    | 22 -> buildOptions |> Seq.find (fun s -> s.geodeBots = state.geodeBots + 1)
//    | 23 -> buildOptions |> Seq.find (fun s -> s.geodeBots = state.geodeBots + 1)
//    | 24 -> buildOptions |> Seq.find (fun s -> s.geodeBots = state.geodeBots + 1)
//    | 26 -> buildOptions |> Seq.find (fun s -> s.geodeBots = state.geodeBots + 1)
//    | 27 -> buildOptions |> Seq.find (fun s -> s.geodeBots = state.geodeBots + 1)
//    | 29 -> buildOptions |> Seq.find (fun s -> s.geodeBots = state.geodeBots + 1)
//    | 30 -> buildOptions |> Seq.find (fun s -> s.geodeBots = state.geodeBots + 1)
//    | 31 -> buildOptions |> Seq.find (fun s -> s.geodeBots = state.geodeBots + 1)
//    | _ -> state
//maxGeodesForBlueprintRec blueprint (optimumState.Update(collected)) (minute + 1)
