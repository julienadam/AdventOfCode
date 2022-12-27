#load "../../Tools.fsx"
open System
open System.IO
open System.Text.RegularExpressions
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
    // let isOpen = (name = "AA")
    name, { name = name; rate = rate; tunnels = tunnels}

let getInput p = 
    File.ReadAllLines(getInputPath2022 p) 
    |> Seq.map mapValveConfiguration
    |> Map.ofSeq
    |> Dump

type ValveSet = Set<Valve>
type PathCostMap = Map<(Valve*Valve), int>

// TODO: memoize released according to (pos, minute, open) ?
// TODO: store best released and cut if no chance to be better even if best remaining valves open ?
let rec solve pos minute (openValves:ValveSet) (closedValves:ValveSet) released (costs:PathCostMap) =
    if minute > 30 then
        failwithf "Went too far !"
    if minute = 30 then
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
            // Get all possible targets and find best route
            // TODO : order using a best estimate heuristic, target big rate valves first ?
            // TODO : cut branches
            reachableClosed |> Seq.map (fun target ->
                let nextOpen = openValves |> Set.add target
                let nextClosed = reachableClosed |> Set.remove target
                let moveAndOpenDuration = costs[pos, target] + 1
                let releasedByOpenValveUntilEnd = (30 - (minute + moveAndOpenDuration - 1)) * target.rate

                solve target (minute + moveAndOpenDuration) nextOpen nextClosed (released + releasedByOpenValveUntilEnd) costs
                )
            |> Seq.max

open FullAStar

let solve1 (valves:Map<string, Valve>) =

    let c = {
        neighbours = fun v -> v.tunnels |> Seq.map (fun a -> valves.[a])
        gCost = fun _ _ -> 1
        fCost = fun _ _ -> 1
        maxIterations = None
    }

    let pathCosts = 
        Seq.allPairs valves valves
        |> Seq.filter (fun (a,b) -> not (a = b))
        |> Seq.map (fun (a,b) -> (a.Value,b.Value), ((FullAStar.search a.Value b.Value c) |> Option.get |> Seq.length) - 1)
        |> Map.ofSeq

    let aa = valves["AA"] 
    let rest = valves.Values |> Set.ofSeq |> Set.remove aa
    solve aa 1 (Set.ofList [aa]) rest 0 pathCosts

getInput "Day16.txt"
|> solve1
