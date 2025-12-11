#time "on"
#load "../../Tools.fs"
#r "nuget: NFluent"

open System.IO
open AdventOfCode
open Checked
open NFluent

let getInput name =
    File.ReadLines(getInputPath2025 name)
    |> Seq.map (fun l -> l |> ssplit ": " |> tupleize2)
    |> Seq.map (fun (device, outputs) -> device, outputs |> ssplit " ")

let countPaths (connectionMap:Map<string, string[]>) from dest =
    let memo = System.Collections.Generic.Dictionary<string, int>()
    
    let rec countPathsRec position =
        match memo.TryGetValue position with
        | true, v -> v
        | false, _ ->
            if position = dest then
                1
            else
                let result = 
                    match connectionMap |> Map.tryFind position with
                    | Some outputs -> outputs |> Seq.sumBy (fun o -> countPathsRec o)
                    | None ->
                        if position = "out" then
                            0
                        else
                            failwithf $"Could not find {position} in map"
                memo[position] <- result
                result
        
    countPathsRec from

let solve1 input =
    let connections = getInput input
    let connectionMap = connections |> Map.ofSeq
    countPaths connectionMap "you" "out"
    
Check.That(solve1 "Day11_sample1.txt").IsEqualTo(5)
solve1 "Day11.txt"

let rec pathExists (connectionMap:Map<string, string[]>) from dest =
    if from = "out" then
        false
    else if from = dest then
        true
    else
        let outputs = connectionMap[from] 
        outputs |> Seq.exists (fun o -> pathExists connectionMap o dest)

let solve2 input =
    let connectionMap = getInput input |> Map.ofSeq
    // determine if fft is before or after dac in graph
    let first, second = if pathExists connectionMap "dac" "fft" then "dac","fft" else "fft","dac"
    // find paths between srv and the first
    let svrToFirst = countPaths connectionMap "svr" first
    // find paths between first and second
    let firstToSecond = countPaths connectionMap first second
    // find paths between second and end
    let secondToEnd = countPaths connectionMap second "out"
    
    int64 svrToFirst * int64 firstToSecond * int64 secondToEnd

Check.That(solve2 "Day11_sample2.txt").IsEqualTo(2)
solve2 "Day11.txt"





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
