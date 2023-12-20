#time "on"
#load "../../Tools.fs"

open System
open System.IO
open AdventOfCode
open System.Collections.Generic

type Pulse = | Low | High

type Mod = 
    | FlipFlop of bool
    | Conjunction of Pulse * Pulse
    | Broadcaster
    | Button

let parseLine line =
    let mdl, dests = line |> ssplit2 " -> "

    let modType = 
        match mdl with
        | "button" -> Button
        | "broadcaster" -> Broadcaster
        | m when m.StartsWith("&") -> Conjunction (Low, Low)
        | m when m.StartsWith("%") -> FlipFlop (false)
        | _ -> failwithf "Unknown module %s" mdl

    mdl, (modType, dests |> ssplit ", " |> Array.toList)



let getInput name = 
    File.ReadAllLines(getInputPath2023 name)
    |> Array.map parseLine
    |> dict


let solve1 input =
    let modules = getInput input |> Dump
    let states = new Dictionary<string, bool*bool>()

    let rec run activeModules =
        match activeModules with
        | [] -> ()
        | _ ->
            let next = 
                activeModules |> List.collect (fun act ->
                    let modType, dests = modules[act]
                    match modType with
                    | Button ->
                        dests |> List.iter(fun d -> states[d] <- (true, false))
                    | Broadcaster ->
                        let low, high = states[act]
                        dests |> List.iter(fun d -> states[d] <- (low, high))
                    | _ -> 
                        failwithf "not implemented"

                    // Remove end modules
                    dests |> List.filter (fun n -> modules.ContainsKey n)
                )
            run next

    run ["button"]
    //     let next = 
    //         activeModules 
    //         |> List.collect (fun name ->
    //             let state, dests = modules[name]
    //             apply state (dests |> List.map (fun n -> modules[n] |> fst))
    //         )

    //     run next
        // match current with
        // | [] -> ()
        // | _ ->
        //     current.
        // match current.modType with
        // | Button, [next] ->
        //     modules[next.name] <- apply next 
        //     ()
        // | Broadcaster, dests ->
        //     dests |> Seq.iter (fun d -> apply current.incomingLow current.incomingHigh)
        //     ()
    ()

solve1 "Day20_sample1.txt"
