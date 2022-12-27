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

let rec solve pos minute (openValves:ValveSet) (closedValves:ValveSet) released (costs:PathCostMap) =
    // Calculate total released at this point
    let totalRate = (openValves |> Seq.sumBy (fun v -> v.rate));
    let nextReleased = totalRate + released
    
    if minute > 30 then
        failwithf "Went too far !"

    if minute = 30 then
        nextReleased
    else
        // Find reachable valves, no point going anywhere if there isn't enough time to open anything
        let reachableClosed = closedValves |> Set.filter(fun target ->
            let moveAndOpenCost = costs[pos, target] + 1
            moveAndOpenCost + minute <= 30)

        // All valves open or unreachable, nothing to do but wait
        // TODO: cut it off by computing rest here
        if reachableClosed.IsEmpty then
            solve pos (minute+1) openValves reachableClosed nextReleased costs
        else
            // Get all possible targets and find best route
            // TODO : order using a best estimate heuristic, target big rate valves first ?
            // TODO : cut branches
            reachableClosed |> Seq.map (fun target ->
                let nextOpen = openValves |> Set.add target
                let nextClosed = reachableClosed |> Set.remove target
                let moveAndOpenCost = costs[pos, target] + 1
                let nextReleased = moveAndOpenCost * totalRate
                solve target (minute + moveAndOpenCost) nextOpen nextClosed nextReleased costs
                )
            |> Seq.max

open FullAStar
            
let solve1 (valves:Map<string, Valve>) =

    let c = {
        neighbours = fun v -> v.tunnels |> Seq.map (fun a -> valves.[a])
        gCost = fun v1 v2 -> 1
        fCost = fun v1 v2 -> 1
        maxIterations = None
    }

    let pathCosts = 
        Seq.allPairs valves valves
        |> Seq.filter (fun (a,b) -> not (a = b))
        |> Seq.map (fun (a,b) -> (a.Value,b.Value), (FullAStar.search a.Value b.Value c) |> Option.get |> Seq.length)
        |> Map.ofSeq

    let aa = valves["AA"] 
    let rest = valves.Values |> Set.ofSeq |> Set.remove aa
    solve aa 1 (Set.ofList [aa]) rest 0 pathCosts

getInput "Day16_sample1.txt"
|> solve1

















//type MemoKey = {
//    position : string
//    minute : int
//    openValves : Set<string>
//}

//let memo = new System.Collections.Generic.Dictionary<MemoKey, int * (string * int) list>()

//let getMemoKey (valves:Map<string, Valve>) pos minute=
//    let openValves = 
//        valves 
//        |> Seq.filter (fun (kvp) -> kvp.Value.isOpen) 
//        |> Seq.map (fun kvp -> kvp.Key) 
//        |> Set.ofSeq
//    { position = pos; minute = minute; openValves = openValves }


//let rec decide currentPosition (valves:Map<string, Valve>) minute (currentFlowRate:int) path =

//    let nextPath = (currentPosition, currentFlowRate) :: path
//    if minute = 30 then
//        currentFlowRate, nextPath
//    else
//        let memoKey = getMemoKey valves currentPosition minute
//        match memo.TryGetValue(memoKey) with
//        | true, v ->
//            // printfn "Using memoized value %A for %s %i flowrate is %i" v currentPosition minute currentFlowRate
//            v
//        | false, _ ->
//            let valve = valves.[currentPosition]
            
//            // Try opening then moving on
//            let maxWithOpening =
//                if not valve.isOpen then
//                    // Open the valve
//                    let nextValves = valves |> Map.add currentPosition { valve with isOpen = true}
//                    let released, pathWithOpen = decide currentPosition nextValves (minute + 1) (currentFlowRate + valve.rate) nextPath
//                    let result = (currentFlowRate + released), pathWithOpen
//                    Some result
//                else
//                    None

//            // Explore all tunnels first
//            let maxWithoutOpening, pathWithoutOpening =
//                let max, bestPath = 
//                    valve.tunnels
//                    |> Seq.map (fun t -> decide t valves (minute + 1) currentFlowRate nextPath) 
//                    |> Seq.maxBy fst
//                let result = (max + currentFlowRate), bestPath
//                result


//            let result = 
//                match maxWithOpening with
//                | Some (m, p) when m > maxWithoutOpening -> m, p
//                | _ -> maxWithoutOpening, pathWithoutOpening

//            let key = getMemoKey valves currentPosition minute
//            memo.Add(key, result)
//            result



//let solve1 input = 
//    decide "AA" input 1 0 []

//getInput "Day16_sample1.txt"
//|> solve1

//getInput "Day16.txt"
//|> solve1

