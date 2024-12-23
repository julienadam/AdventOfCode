#time "on"
#load "../../Tools.fs"

open System
open System.IO
open AdventOfCode
open Checked
#r "nuget: Quikgraph"
open QuikGraph

let getInput name = 
    File.ReadAllLines(getInputPath2024 name)
    |> Seq.map (ssplit "-")

let findTriangles (g:BidirectionalGraph<string, UndirectedEdge<string>>) = seq {
    for e in g.Edges |> Seq.filter (fun e -> e.Source[0] = 't' || e.Target[0] = 't') do
        let u = e.Source
        let v = e.Target
        for w in g.Vertices do
            if g.Edges |> Seq.exists (fun e -> (e.Source = min v w && e.Target = max v w))
                && g.Edges |> Seq.exists (fun e -> (e.Source = min w u && e.Target = max w u)) then
                [u;v;w] |> List.sort
    }

let solve1 input =
    let g = new QuikGraph.BidirectionalGraph<string, UndirectedEdge<string>>()
    let pairs = getInput input
    for [|a;b|] in pairs do 
        g.AddVertex(a) |> ignore
        g.AddVertex(b) |> ignore
        g.AddEdge(new UndirectedEdge<string>(min a b, max a b)) |> ignore
    
    findTriangles g |> Seq.distinct |> Seq.length

solve1 "Day23_sample1.txt"
solve1 "Day23.txt"

open System.Collections.Generic;

let buildEdgeDict (edges:('a*'a) seq) =
    edges 
    |> Seq.collect (fun (a,b) -> [(a,b); (b,a)])
    |> Seq.filter (fun (a,b) -> a <> b)
    |> Seq.distinct
    |> Seq.groupBy fst
    |> Seq.map (fun (k,v) -> k, v |> Seq.map snd |> Seq.toArray)
    |> Map.ofSeq

let neighbors v (edges:Map<'a,'a array>) = edges[v] |> Set.ofArray

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
