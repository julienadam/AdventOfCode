#time "on"
#load "../../Tools.fs"
#load "../../Tools/SeqEx.fs"
#load "../../Tools/Distance.fs"
#r "nuget: NFluent"

open System
open System.IO
open AdventOfCode
open Checked
open NFluent

let getInput name =
    File.ReadAllLines(getInputPath2025 name)
    |> Seq.map splitIntList64
    |> Seq.map tupleize3
    |> Seq.toArray

type JunctionBox = int64*int64*int64
type CircuitId = int

let connectCircuits junctionBoxes numberOfConnectionsToMake =
    // List of sets ? one set per circuit
    // iter on all boxes
    // find 2 closest with d > previousD
    // if no set / no set, create new set put both inside
    // if set1 / no set, add B to set1
    // if set1 / set2, merge set1 and set 2
        
    let rec connect (circuits:Map<JunctionBox, Option<CircuitId>>) (remainingConnections:int) =
        
        ()
    connect (Map.empty) numberOfConnectionsToMake

let solve1 numBoxes input =
    let boxes = getInput input
    let closestBoxes =
        boxes
        |> SeqEx.autoProduct
        |> Seq.where (fun (a,b) -> a <> b)
        |> Seq.map (fun (a,b) -> (a,b), Distance.euclidianDistance3D a b)
        |> Seq.sortBy snd
        |> Seq.take numBoxes
        // |> Seq.toArray |> Dump
    
    let mutable circuits : List<System.Collections.Generic.HashSet<JunctionBox>> = List.Empty
    closestBoxes
    |> Seq.iter (fun ((ja,jb), _) ->
        printfn $"Next closest : {ja} and {jb}"
        match circuits |> List.tryFind (fun c -> c.Contains(ja)), circuits |> List.tryFind (fun c -> c.Contains(jb)) with
        | Some s, None -> s.Add(jb) |> ignore
        | None, Some s -> s.Add(ja) |> ignore
        | None, None ->
            let s = new System.Collections.Generic.HashSet<JunctionBox>()
            s.Add(ja) |> ignore
            s.Add(jb) |> ignore
            circuits <- s :: circuits
        | Some sA, Some sB ->
            sA.UnionWith(sB)
            circuits <- (circuits |> List.filter (fun x -> x <> sB))
        circuits |> List.map (fun s -> s |> Seq.toArray) |> Dump |> ignore
        )
    circuits |> List.map (fun s -> s |> Seq.toArray)
    
solve1 10 "Day08_sample1.txt"
solve1 1000 "Day08.txt"
