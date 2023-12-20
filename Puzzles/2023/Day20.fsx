#time "on"
#load "../../Tools.fs"
#r "nuget: NFluent"

open System
open System.IO
open AdventOfCode
open System.Collections.Generic
open NFluent

type Pulse = | Low | High

type ModState = 
    | FlipFlop of bool
    | Conjunction of Map<string, Pulse>
    | Broadcaster

type Module =
    {
        name  : string
        state : ModState
        dests  : string list
    }
    override this.ToString() =
        sprintf "%s : %A" this.name this.state

let parseLine line =
    let mdl, dests = line |> ssplit2 " -> "

    let name, modType = 
        match mdl with
        | "broadcaster" -> "broadcaster", Broadcaster
        | m when m.StartsWith("&") -> m.Substring(1), Conjunction Map.empty
        | m when m.StartsWith("%") -> m.Substring(1), FlipFlop (false)
        | _ -> failwithf "Unknown module %s" mdl

    name, { name = name; state = modType; dests = dests |> ssplit ", " |> Array.toList }

let getInput name = 
    File.ReadAllLines(getInputPath2023 name)
    |> Array.map parseLine
    |> Array.toList

let runOnce (modules:Dictionary<string, Module>) first = 
    let mutable high = 0L
    let mutable low = 0L

    let q = new Queue<string * string * Pulse>()
    q.Enqueue("button", first, Low)
    while q.Count > 0 do
        let sender, receiver, pulse = q.Dequeue()
        // printfn "%s -%A-> %s" sender pulse receiver
        match pulse with
        | High -> high <- high + 1L
        | Low -> low <- low + 1L

        if modules.ContainsKey(receiver) = false then
            () // Skip output modules
        else
            let rcvMod = modules[receiver]
            match rcvMod.state, pulse with
            | Broadcaster, p -> 
                for d in rcvMod.dests do
                    q.Enqueue(receiver, d, p)
            | FlipFlop _, High ->
                ()
            | FlipFlop true, Low ->
                modules[receiver] <- { modules[receiver] with state = FlipFlop false }
                for d in rcvMod.dests do
                    q.Enqueue(receiver, d, Low)
            | FlipFlop false, Low ->
                modules[receiver] <- { modules[receiver] with state = FlipFlop true }
                for d in rcvMod.dests do
                    q.Enqueue(receiver, d, High)
            | Conjunction map, _ ->
                // Update the conjunction's memory
                let newMap = map |> Map.add sender pulse
                modules[receiver] <- { modules[receiver] with state = Conjunction newMap }
                // And THEN check the output signal
                let allHigh = newMap.Values |> Seq.forall (fun v -> v = High)
                let signal = if allHigh then Low else High
                for d in rcvMod.dests do
                    q.Enqueue(receiver, d, signal)
    high, low

let solve runs name =
    let modules = getInput name
    let broadcaster, _ = modules |> Seq.find (fun (_, m) -> m.state = Broadcaster)

    let getSources name = 
        modules 
        |> Seq.filter(fun (_,mdl) -> mdl.dests |> List.contains(name))
        |> Seq.map (fun (n,_) -> n, Low)
        |> Map.ofSeq

    let modules = 
        modules
        |> Seq.map (fun (name, m) ->
            // Initialize conjunctions
            match m.state with 
            | Conjunction _ -> name, { m with state = Conjunction (getSources m.name) }
            | _ -> (name, m)
        ) 
        |> Seq.map (fun (k,v) -> KeyValuePair(k,v)) 
        |>  Dictionary


    let (++) (a,b) (c,d) = (a + c, b + d)

    [1..runs] 
    |> Seq.fold 
        (fun (h,l) _ -> (runOnce modules broadcaster) ++ (h,l))
        (0L,0L)

let solve1 input = 
    let high, low = solve 1000 input
    high * low

Check.That(solve 1000 "Day20_sample1.txt").IsEqualTo((4000L, 8000L))
Check.That(solve 1000 "Day20_sample2.txt").IsEqualTo((2750L, 4250L))

solve1 "Day20.txt"