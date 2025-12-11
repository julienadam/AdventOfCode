#time "on"
#load "../../Tools.fs"
#r "nuget: NFluent"

open System
open System.IO
open AdventOfCode
open Checked
open NFluent

let getInput name =
    File.ReadLines(getInputPath2025 name)
    |> Seq.map (fun l -> l |> ssplit ": " |> tupleize2)
    |> Seq.map (fun (device, outputs) -> device, outputs |> ssplit " ")

#r "nuget: Quikgraph"
#r "nuget: Quikgraph.GraphViz"

open QuikGraph
open QuikGraph.Graphviz.Dot
open QuikGraph.Graphviz

let display connections =
    let g = EdgeListGraph<string, TaggedEdge<string, string>>()
    for device, outputs in connections do
        for output in outputs do
            g.AddEdge(TaggedEdge<string, string>(device, output, output)) |> ignore
        
    let dotGraph = g.ToGraphviz(fun algo ->
        algo.CommonVertexFormat.Shape <- GraphvizVertexShape.Diamond
        algo.FormatVertex.Add(fun args -> args.VertexFormat.Label <- $"{args.Vertex}")
    )
    
    let root =  Path.Combine(Path.GetTempPath(), "aoc")
    Directory.CreateDirectory(root) |> ignore
    let tempFile = Path.Combine(root, "aoc_2025_day11.dot")
    File.WriteAllText(tempFile, dotGraph)

let solve1 input =
    let connections = getInput input
    let connectionMap = connections |> Map.ofSeq
    // display connections
    
    let rec countPaths position =
        if position = "out" then
            1
        else
            let outputs = connectionMap[position] 
            outputs |> Seq.sumBy (fun o -> countPaths o)
        
    countPaths "you"
    
Check.That(solve1 "Day11_sample1.txt").IsEqualTo(5)
solve1 "Day11.txt"
