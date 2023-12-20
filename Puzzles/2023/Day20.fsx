#time "on"
#load "../../Tools.fs"
#load "../../Tools/MathEx.fs"
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

let runOnce (modules:Dictionary<string, Module>) first spyTarget round = 
    let mutable high = 0L
    let mutable low = 0L
    let mutable targets = List<string * int>()

    let q = new Queue<string * string * Pulse>()
    q.Enqueue("button", first, Low)
    while q.Count > 0 do
        let sender, receiver, pulse = q.Dequeue()
        // printfn "%s -%A-> %s" sender pulse receiver
        match pulse with
        | High -> high <- high + 1L
        | Low -> low <- low + 1L

        if pulse = High && spyTarget = receiver then
            targets.Add(sender, round)

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
    (high, low), targets

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

    [1..runs] 
    |> Seq.fold 
        (fun (h,l) r -> 
            let (hh,ll), _ = (runOnce modules broadcaster "" r)
            hh + h, ll + l
            )
        (0L,0L)

let solve1 input = 
    let high, low = solve 1000 input
    high * low

Check.That(solve 1000 "Day20_sample1.txt").IsEqualTo((4000L, 8000L))
Check.That(solve 1000 "Day20_sample2.txt").IsEqualTo((2750L, 4250L))

solve1 "Day20.txt"

#r "nuget: GiGraph.Dot"

open GiGraph.Dot.Entities.Graphs
open GiGraph.Dot.Extensions
open GiGraph.Dot.Types.Nodes

let printGraph name =
    let modules = getInput name
    let graph = new DotGraph(directed = true)
    let broadcaster, _ = modules |> Seq.find (fun (_, m) -> m.state = Broadcaster)
    graph.Nodes.Add(broadcaster, fun n -> n.Shape <- DotNodeShape.Ellipse) |> ignore

    for (n, m) in modules do
        if n <> broadcaster then
            graph.Nodes.Add(n, fun n ->
                match m.state with
                | Broadcaster -> n.Shape <- DotNodeShape.Diamond
                | Conjunction _ -> n.Shape <- DotNodeShape.Triangle
                | FlipFlop _ -> n.Shape <- DotNodeShape.Rectangle
            ) |> ignore
        for d in m.dests do
            graph.Edges.Add(n, d) |> ignore

    File.WriteAllText(Path.ChangeExtension(name, ".dot"), graph.Build())

printGraph "Day20.txt"

open AdventOfCode.MathEx

let solve2 name =
    let modules = getInput name
    let broadcaster, _ = modules |> Seq.find (fun (_, m) -> m.state = Broadcaster)
    let getSources name = 
        modules 
        |> Seq.filter(fun (_,mdl) -> mdl.dests |> List.contains(name))
        |> Seq.map (fun (n,_) -> n, Low)
        |> Map.ofSeq

    let rxPrec = (getSources "rx").Keys |> Seq.head
    let rxSources = getSources(rxPrec).Keys |> Set.ofSeq

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

    // Quick and dirty
    let highDetected = new Dictionary<string, int>()
    let cyclesDetected = new Dictionary<string, int>()
    let rec hunt presses =
        if cyclesDetected.Count = rxSources.Count then
            ()
        else
            let _, found = runOnce modules broadcaster rxPrec presses
            for (name, round) in found do
                if cyclesDetected.ContainsKey(name) = false && highDetected.ContainsKey(name) = true then
                    cyclesDetected.Add(name, round - highDetected[name])
                else if highDetected.ContainsKey(name) = false then
                    highDetected.Add(name, round)
            hunt (presses + 1)
    hunt 0

    cyclesDetected.Values
    |> Seq.map int64
    |> Seq.fold lcm64 1L


solve2 "Day20.txt"