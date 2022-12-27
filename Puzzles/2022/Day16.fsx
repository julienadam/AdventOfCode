#time
#load "../../Tools.fsx"
#r "nuget: FSharp.Collections.ParallelSeq"

open System
open System.IO
open System.Text.RegularExpressions
open FSharp.Collections.ParallelSeq
open Checked
open Tools

let regexValve = Regex(@"Valve (?<v>[A-Z]{2}) has flow rate=(?<rate>\d+); tunnels? leads? to valves? (?<leadsTo>.*)")

type Valve = {
    name : string
    rate : int
    tunnels : string[]
}

let mapValveConfiguration line =
    let m = regexValve.Match line
    if not m.Success then
        failwithf "Could not parse line %s" line
    let name = m |> mStr "v"
    let rate = m |> mInt "rate"
    let tunnels = m |> mStr "leadsTo" |> ssplit ", "
    name, { name = name; rate = rate; tunnels = tunnels}

let getInput p = 
    File.ReadAllLines(getInputPath2022 p) 
    |> Seq.map mapValveConfiguration
    |> Map.ofSeq
    |> Dump

type ValveSet = Set<Valve>
type PathCostMap = Map<(Valve*Valve), int>

open FullAStar

let getPathCosts (valves:Map<string, Valve>) =
    let c = {
        neighbours = fun v -> v.tunnels |> Seq.map (fun a -> valves.[a])
        gCost = fun _ _ -> 1
        fCost = fun _ _ -> 1
        maxIterations = None
    }

    Seq.allPairs valves valves
    |> Seq.filter (fun (a,b) -> not (a = b))
    |> Seq.map (fun (a,b) -> (a.Value,b.Value), ((FullAStar.search a.Value b.Value c) |> Option.get |> Seq.length) - 1)
    |> Map.ofSeq

let getNonZeroValves (valves:Map<string, Valve>) = 
    valves.Values 
    |> Seq.filter (fun v -> v.rate > 0) // Remove 0 flow targets, including AA
    |> Set.ofSeq 

module Part1 =
    // For tracking
    let mutable explored = 0
    let mutable cut = 0
    
    let findBestRoute pos minute (openValves:ValveSet) (closedValves:ValveSet) (costs:PathCostMap) =
        let mutable best = 0
        let rec findBestRouteRec pos minute (openValves:ValveSet) (closedValves:ValveSet) released (costs:PathCostMap) =
            explored <- explored + 1
            if minute > 30 then
                failwithf "Went too far !"
            if minute = 30 then
                if released > best then
                    best <- released
                released
            else
                // Find reachable valves, no point going anywhere if there isn't enough time to open anything
                let reachableClosed = closedValves |> Set.filter(fun target ->
                    let moveAndOpenCost = costs[pos, target] + 1
                    moveAndOpenCost + minute <= 30)

                // All valves open or unreachable, nothing to do, return the total
                if reachableClosed.IsEmpty then
                    released
                else
                    // Early cutoff if all closedValves open until end of 30 minutes wouldn't be better than current best
                    let maxRemainingMoves = (30 - minute) / 2
                    let bestPossibleRate = 
                        closedValves 
                        |> Seq.sortByDescending (fun v -> v.rate) 
                        |> Seq.take (min maxRemainingMoves closedValves.Count)
                        |> Seq.sumBy (fun v -> v.rate)
                    let bestPossibleResult = (30 - minute) * bestPossibleRate + released
                    if bestPossibleResult < best then
                        cut <- cut + 1
                        0
                    else
                        // Get all possible targets and find best route
                        // TODO : order using a best estimate heuristic, target big rate valves first ?
                        reachableClosed |> Seq.map (fun target ->
                            let nextOpen = openValves |> Set.add target
                            let nextClosed = reachableClosed |> Set.remove target
                            let moveAndOpenDuration = costs[pos, target] + 1
                            let releasedByOpenValveUntilEnd = (30 - (minute + moveAndOpenDuration - 1)) * target.rate

                            findBestRouteRec target (minute + moveAndOpenDuration) nextOpen nextClosed (released + releasedByOpenValveUntilEnd) costs
                            )
                        |> Seq.max
        
        findBestRouteRec pos minute openValves closedValves 0 costs

    let solve1 (valves:Map<string, Valve>) =
        let pathCosts = getPathCosts valves
        let aa = valves["AA"]
        let rest = getNonZeroValves valves
        findBestRoute aa 1 (Set.ofList [aa]) rest pathCosts

getInput "Day16.txt"
|> Part1.solve1

module Part2 =
    // Find all the ways a set can be split in 2 approximately equal sized sets
    let enumHalfSplits (input:Set<'a>) =
        SeqEx.combinations [] (input.Count / 2) (input |> Set.toList)
        |> Seq.map (fun a -> 
            let combination = a |> Set.ofList
            combination, Set.difference input combination
        )

    let solve2 (valves:Map<string, Valve>) =
        
        let sw = System.Diagnostics.Stopwatch.StartNew()
        let pathCosts = getPathCosts valves
        printfn "Computing costs : %A" sw.Elapsed
        let aa = valves["AA"]
        let rest = getNonZeroValves valves
        let splits = enumHalfSplits rest |> Seq.toList
        
        splits
        |> PSeq.map (fun (setForMe, setForElephant) -> 
            Part1.findBestRoute aa 5 (Set.ofList [aa]) setForMe pathCosts +
            Part1.findBestRoute aa 5 (Set.ofList [aa]) setForElephant pathCosts
        )
        |> Seq.max

let sw = System.Diagnostics.Stopwatch.StartNew()

getInput "Day16.txt"
|> Part2.solve2 

printfn "Total runtime: %A" sw.Elapsed



