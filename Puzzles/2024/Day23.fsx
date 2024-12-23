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

let findTriangles (g) =
    
    for e in g.Edges |> Seq.filter (fun e -> e.Source[0] = 't' || e.Target[0] = 't') do
        let u = e.Source
        let v = e.Target
        for w in g.Vertices do
            if g.Edges |> Seq.exists (fun e -> (e.Source = min v w && e.Target = max v w))
                && g.Edges |> Seq.exists (fun e -> (e.Source = min w u && e.Target = max w u)) then
                [u;v;w] 

let solve1 input =
    let g = new QuikGraph.BidirectionalGraph<string, UndirectedEdge<string>>()
    let pairs = getInput input
    for [|a;b|] in pairs do 
        g.AddVertex(a) |> ignore
        g.AddVertex(b) |> ignore
        g.AddEdge(new UndirectedEdge<string>(min a b, max a b)) |> ignore


solve1 "Day23_sample1.txt"



//let algo = QuikGraph.Algorithms.ConnectedComponents.StronglyConnectedComponentsAlgorithm(g)
//algo.