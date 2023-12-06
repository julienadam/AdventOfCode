#time
#load "../../Tools.fs"
#load "../../Tools/RegexTools.fs"
#r "nuget: FSharp.Collections.ParallelSeq"

open System.Text.RegularExpressions
open System.IO
open System.Threading
open System.Collections.Generic
open FSharp.Collections.ParallelSeq
open Checked
open AdventOfCode

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
    skippedOre : bool
    skippedClay : bool
    skippedObsidian : bool
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
    member this.BuyObsidianBot(orePrice, clayPrice) =
        {
            this with 
                obsidianBots = this.obsidianBots + 1
                ore = this.ore - orePrice
                clay = this.clay - clayPrice
                skippedObsidian = false; skippedClay = false; skippedOre = false
        }
    member this.BuyClayBot(orePrice) = 
        { 
            this with 
                clayBots = this.clayBots + 1
                ore = this.ore - orePrice 
                skippedObsidian = false; skippedClay = false; skippedOre = false
        }
    member this.BuyOreBot(orePrice) = 
        { 
            this with 
                oreBots = this.oreBots + 1
                ore = this.ore - orePrice 
                skippedObsidian = false; skippedClay = false; skippedOre = false
        }
    member this.BuyGeodeBot(orePrice, obsidianPrice) =
        { 
            this with
                geodeBots = this.geodeBots + 1
                ore = this.ore - orePrice
                obsidian = this.obsidian - obsidianPrice 
                skippedObsidian = false; skippedClay = false; skippedOre = false
        }

let initState () = {
    ore = 0; clay = 0; obsidian = 0; geode = 0
    oreBots = 1; clayBots  = 0; obsidianBots = 0; geodeBots = 0
    skippedOre = false; skippedClay = false; skippedObsidian = false
}

type Constraints = {
    maxUsableOreBots : int
    maxUsableClayBots : int
    maxUsableObsidianBots : int
}

let pickBuildOptions (blueprint:Blueprint) (state:State) (constraints:Constraints)= seq {
    let shouldBuyObsidian (ore, clay) =
        state.ore >= ore 
        && state.clay >= clay 
        && state.obsidianBots < constraints.maxUsableObsidianBots 
        && (not state.skippedObsidian)

    let shoudBuyClay ore =
        state.ore >= ore 
        && state.clayBots < constraints.maxUsableClayBots 
        && (not state.skippedClay)

    let shouldBuyOre ore =
        state.ore >= ore 
        && state.oreBots < constraints.maxUsableOreBots 
        && (not state.skippedOre)

    match blueprint.geode with
    | ore, obsidian when state.ore >= ore && state.obsidian >= obsidian ->
        yield state.BuyGeodeBot(ore, obsidian)
    | _ ->
        let canBuildObs = if shouldBuyObsidian blueprint.obsidian then Some (state.BuyObsidianBot blueprint.obsidian) else None
        if canBuildObs.IsSome then
            yield canBuildObs.Value

        let canBuildClay = if shoudBuyClay(blueprint.clay) then Some (state.BuyClayBot(blueprint.clay)) else None
        if canBuildClay.IsSome then
            yield canBuildClay.Value

        let canBuildOre = if shouldBuyOre(blueprint.ore) then Some (state.BuyOreBot(blueprint.ore)) else None
        if canBuildOre.IsSome then
            yield canBuildOre.Value

        // Don't wait if all options are available
        if canBuildObs.IsNone || canBuildClay.IsNone || canBuildOre.IsNone then
            yield { state with skippedObsidian = canBuildObs.IsSome; skippedClay = canBuildClay.IsSome; skippedOre = canBuildOre.IsSome }
}

// TODO : more heuristics ?

let producableUntilEnd n =
    [1..n] 
    |> Seq.fold (fun s i -> s + i) 0

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
        printfn "Blueprint %i explored %i, memo size %i, memo hit count %i, best %i cut %i" blueprint.id counter memo.Count hitCount best cut
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
                    let _,_,_,ng = collected
                    // Cut short if not a chance to best the leading score
                    let remainingTurns = maxMinutes - minute
                    let bestPossibleScore = 
                        state.geode + ng + // Current geodes, including the ones from this turn
                        (state.geodeBots * remainingTurns) + // current bots mining until end
                        (producableUntilEnd (remainingTurns)) // 1 more bot until the end producing
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
            memo.Add((state, minute), result)
            result
    maxGeodesForBlueprintRec blueprint (initState()) 1 


let solve1 blueprints = 
    blueprints
    |> PSeq.map (fun bp -> bp.id * (maxGeodesForBlueprint bp 24))
    |> Seq.sum


//getInput "Day19.txt"
//|> solve1

let bp1 = 
    getInput "Day19_sample1.txt"
    |> Seq.head
let bp2 = 
    getInput "Day19_sample1.txt"
    |> Seq.skip 1
    |> Seq.head

//maxGeodesForBlueprint bp1 32
//maxGeodesForBlueprint bp2 32

let solve2 blueprints =
    let result = 
        blueprints 
        |> Seq.take 3
        |> PSeq.map (fun bp -> maxGeodesForBlueprint bp 32)
        |> Seq.toArray
    result.[0] * result.[1] * result.[2]

getInput "Day19.txt"
|> solve2


