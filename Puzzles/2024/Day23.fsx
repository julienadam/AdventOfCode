#time "on"
#load "../../Tools.fs"

open System
open System.IO
open AdventOfCode
open Checked

let getInput name = 
    File.ReadAllLines(getInputPath2024 name)
    |> Seq.map (ssplit "-")
    
open System.Collections.Generic;

let buildEdgeSet (edges:('a*'a) seq) =
    edges 
    |> Seq.collect (fun (a,b) -> [(a,b); (b,a)])
    |> Seq.filter (fun (a,b) -> a <> b)
    |> Set.ofSeq

let buildEdgeDict (edges:('a*'a) seq) =
    edges 
    |> Seq.collect (fun (a,b) -> [(a,b); (b,a)])
    |> Seq.filter (fun (a,b) -> a <> b)
    |> Seq.distinct
    |> Seq.groupBy fst
    |> Seq.map (fun (k,v) -> k, v |> Seq.map snd |> Seq.toArray)
    |> Map.ofSeq

let neighbors v (edges:Map<'a,'a array>) = edges[v] |> Set.ofArray

let findTriangles (edges:(string*string) seq) = seq {
    let edgeSet = buildEdgeSet edges
    let vertices = edges |> Seq.collect (fun (a,b) -> [a;b]) |> Seq.distinct |> Seq.toList
    for (u,v) in edgeSet |> Seq.filter (fun (u,v) -> u[0] = 't' || v[0] = 't')do
        for w in vertices do
            if (edgeSet.Contains(v,w) || edgeSet.Contains(w,v)) && (edgeSet.Contains(w,u) || edgeSet.Contains(u,w)) then
                [u;v;w] |> List.sort
    }

let solve1 input =
    let pairs = getInput input
    let edges = pairs |> Seq.map tupleize2
    findTriangles edges |> Seq.distinct |> Seq.length

solve1 "Day23_sample1.txt"
solve1 "Day23.txt"


// https://fr.wikipedia.org/wiki/Algorithme_de_Bron-Kerbosch
let BronKerbosh (vertices:'a Set) (edges:Map<'a,'a array>)=
    let found = new List<'a Set>()
    let rec BronKerboshRec (r:'a Set) (p:'a Set) (x:'a Set) =
        if p.IsEmpty && x.IsEmpty then
            found.Add(r)
        else
            let mutable x' = x
            let mutable p' = p
            while not (p'.IsEmpty) do
                let v = p' |> Seq.head
                let nr = r |> Set.add v
                let nv = neighbors v edges
                BronKerboshRec nr (p' |> Set.intersect nv) (x' |> Set.intersect nv)
                p' <- p' |> Set.remove v
                x' <- x' |> Set.add v
    BronKerboshRec Set.empty vertices Set.empty
    found

let solve2 input =
    let pairs = getInput input |> Seq.map tupleize2 |> Dump
    let edges = buildEdgeDict pairs
    let vertices = edges |> Map.keys |> Set.ofSeq
    let cliques = BronKerbosh vertices edges
    let maxClique = cliques |> Seq.maxBy (fun c -> c |> Set.count)
    String.Join(",", maxClique |> Seq.sort |> Seq.toArray)

solve2 "Day23_sample1.txt"
solve2 "Day23.txt"
