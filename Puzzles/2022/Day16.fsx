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
    isOpen : bool
    tunnels : string[]
}

let mapValveConfiguration line =
    let m = regexValve.Match line
    if not m.Success then
        failwithf "Could not parse line %s" line
    let name = m |> mStr "v"
    let rate = m |> mInt "rate"
    let tunnels = m |> mStr "leadsTo" |> ssplit ", "
    let isOpen = (name = "AA")
    name, { name = name; rate = rate; isOpen = isOpen; tunnels = tunnels}

let getInput p = 
    File.ReadAllLines(getInputPath2022 p) 
    |> Seq.map mapValveConfiguration
    |> Map.ofSeq
    |> Dump

//let input = getInput "Day16.txt"

//Seq.allPairs input input
//|> Seq.filter (fun (a,b) -> not (a = b))
//|> Seq.map (fun (a,b) -> a.Key + " -> " + b.Key)
//|> Seq.length



type MemoKey = {
    position : string
    minute : int
    openValves : Set<string>
}

let memo = new System.Collections.Generic.Dictionary<MemoKey, int * (string * int) list>()

let getMemoKey (valves:Map<string, Valve>) pos minute=
    let openValves = 
        valves 
        |> Seq.filter (fun (kvp) -> kvp.Value.isOpen) 
        |> Seq.map (fun kvp -> kvp.Key) 
        |> Set.ofSeq
    { position = pos; minute = minute; openValves = openValves }


let rec decide currentPosition (valves:Map<string, Valve>) minute (currentFlowRate:int) path =

    let nextPath = (currentPosition, currentFlowRate) :: path
    if minute = 30 then
        currentFlowRate, nextPath
    else
        let memoKey = getMemoKey valves currentPosition minute
        match memo.TryGetValue(memoKey) with
        | true, v ->
            // printfn "Using memoized value %A for %s %i flowrate is %i" v currentPosition minute currentFlowRate
            v
        | false, _ ->
            let valve = valves.[currentPosition]
            
            // Try opening then moving on
            let maxWithOpening =
                if not valve.isOpen then
                    // Open the valve
                    let nextValves = valves |> Map.add currentPosition { valve with isOpen = true}
                    let released, pathWithOpen = decide currentPosition nextValves (minute + 1) (currentFlowRate + valve.rate) nextPath
                    let result = (currentFlowRate + released), pathWithOpen
                    Some result
                else
                    None

            // Explore all tunnels first
            let maxWithoutOpening, pathWithoutOpening =
                let max, bestPath = 
                    valve.tunnels
                    |> Seq.map (fun t -> decide t valves (minute + 1) currentFlowRate nextPath) 
                    |> Seq.maxBy fst
                let result = (max + currentFlowRate), bestPath
                result


            let result = 
                match maxWithOpening with
                | Some (m, p) when m > maxWithoutOpening -> m, p
                | _ -> maxWithoutOpening, pathWithoutOpening

            let key = getMemoKey valves currentPosition minute
            memo.Add(key, result)
            result



let solve1 input = 
    decide "AA" input 1 0 []

getInput "Day16_sample1.txt"
|> solve1

getInput "Day16.txt"
|> solve1

