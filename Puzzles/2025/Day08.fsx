#time "on"
#load "../../Tools.fs"
#load "../../Tools/SeqEx.fs"
#load "../../Tools/Distance.fs"
#r "nuget: NFluent"

open System
open System.Diagnostics
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

let solve1 numBoxes input =
    let boxes = getInput input
    let closestBoxes =
        boxes
        |> SeqEx.autoProduct
        |> Seq.where (fun (a,b) -> a <> b)
        |> Seq.map (fun (a,b) -> (a,b), Distance.euclidianDistance3D a b)
        |> Seq.sortBy snd
        |> Seq.take numBoxes
    
    let mutable circuits : List<System.Collections.Generic.HashSet<JunctionBox>> = List.Empty
    closestBoxes
    |> Seq.iter (fun ((ja,jb), _) ->
        match circuits |> List.tryFind (fun c -> c.Contains(ja)), circuits |> List.tryFind (fun c -> c.Contains(jb)) with
        | Some s, None -> s.Add(jb) |> ignore
        | None, Some s -> s.Add(ja) |> ignore
        | None, None ->
            let s = new System.Collections.Generic.HashSet<JunctionBox>()
            s.Add(ja) |> ignore
            s.Add(jb) |> ignore
            circuits <- s :: circuits
        | Some sA, Some sB ->
            if sA <> sB then
                sA.UnionWith(sB)
                circuits <- (circuits |> List.filter (fun x -> x <> sB))
            else
                ()
        )
    
    let bestCircuits =
        circuits
        |> List.sortByDescending (fun s -> s.Count)
        |> List.take 3
        |> List.map (fun s -> s.Count)
        |> List.toArray
        
    bestCircuits[0] * bestCircuits[1] * bestCircuits[2] 
    
    
Check.That(solve1 10 "Day08_sample1.txt").IsEqualTo(40)
solve1 1000 "Day08.txt"

let solve2 input =
    let sw = Stopwatch.StartNew()
    let boxes = getInput input
    printfn $"Input processing : {sw}"
    let closestBoxes =
        boxes
        |> SeqEx.autoProduct
        |> Seq.where (fun (a,b) -> a <> b)
        |> Seq.map (fun (a,b) -> (a,b), Distance.euclidianDistance3D a b)
        |> Seq.sortBy snd
        |> Seq.map fst
        |> Seq.toList
    printfn $"Sorting by distance : {sw}"
    let mutable circuits : List<System.Collections.Generic.HashSet<JunctionBox>> = List.Empty
    
    let rec buildCircuit remainingBoxes =
        match remainingBoxes with
        | [] -> failwith "Should not be empty"
        | (ja, jb) :: tail ->
            match circuits |> List.tryFind (fun c -> c.Contains(ja)), circuits |> List.tryFind (fun c -> c.Contains(jb)) with
            | Some s, None -> s.Add(jb) |> ignore
            | None, Some s -> s.Add(ja) |> ignore
            | None, None ->
                let s = new System.Collections.Generic.HashSet<JunctionBox>()
                s.Add(ja) |> ignore
                s.Add(jb) |> ignore
                circuits <- s :: circuits
            | Some sA, Some sB ->
                if sA <> sB then
                    sA.UnionWith(sB)
                    circuits <- (circuits |> List.filter (fun x -> x <> sB))
            if circuits.Length = 1 && circuits.Head.Count = boxes.Length then
                let (x1,_,_) = ja
                let (x2,_,_) = jb
                x1 * x2
            else
                buildCircuit tail
    
    let result = buildCircuit closestBoxes 
    printfn $"Building single circuit: {sw}"
    result
    
Check.That(solve2 "Day08_sample1.txt").IsEqualTo(25272)

solve2 "Day08.txt"