open System.Collections.Generic
open System.Text

#time "on"
#load "../../Tools.fs"

open System
open System.IO
open AdventOfCode
open Checked

let getInput name = 
    let (vars, ops) = 
        File.ReadAllText(getInputPath2024 name).Replace("\r", "")
        |> ssplit "\n\n"
        |> tupleize2

    let varList = vars |> ssplit "\n" |> Seq.map (fun line -> line |> ssplit ":" |> tupleize2)
    let opList = ops |> ssplit "\n" |> Seq.map (fun line -> line |> ssplit " " |> Array.filter (fun s -> s <> "->"))
    varList |> Seq.map (fun (v,n) -> v, n |> int), opList |> Seq.map tupleize4

type Operation = string*string*string*string

let bitsToNum (bits:int seq) = 
    // Lazy hack, should have bit wrangled but can't be bothered
       let sb = new StringBuilder()
       for b in bits do
           sb.Append(b) |> ignore
       Int64.Parse(sb.ToString(), Globalization.NumberStyles.BinaryNumber)

let varsToNumber (varValuesMap:Dictionary<string, int>) (prefix:string) =
    varValuesMap 
    |> Seq.filter(fun kvp -> kvp.Key.StartsWith prefix)
    |> Seq.map(fun kvp -> kvp.Key.Substring(1) |> int, kvp.Value)
    |> Seq.sortByDescending fst
    |> Seq.map snd
    |> bitsToNum

let solve1 input =
    let vars, ops = getInput input
    let varValuesMap = new Dictionary<string, int>(vars |> Seq.map (fun (a,b) -> new KeyValuePair<string, int>(a,b)))
    let opsQueue = new Queue<Operation>(ops)

    while opsQueue.Count > 0 do
        let (left, op, right, target) = opsQueue.Dequeue()
        match varValuesMap.TryGetValue(left), varValuesMap.TryGetValue(right) with
        | (true, l), (true, r) ->
            let result = 
                match op with
                | "AND" -> l &&& r
                | "OR" -> l ||| r
                | "XOR" -> l ^^^ r
                | x -> failwithf "Invalid op %s" x
            if varValuesMap.TryAdd(target, result) = false then failwithf "Overwrite"
        | _ -> opsQueue.Enqueue((left, op, right, target))

    varsToNumber varValuesMap "z"

#r "nuget: NFluent"
open NFluent

Check.That(solve1 "Day24_sample1.txt").Equals(4)
Check.That(solve1 "Day24_sample2.txt").Equals(2024)
solve1 "Day24.txt"

let solve2 input = 
    let var, ops = getInput "Day24.txt"

    let isX i s = s = $"x{i:d2}"
    let isY i s = s = $"y{i:d2}"
    let isZ i s = s = $"z{i:d2}"
    let is_a_x (s:string) = s.StartsWith("x")
    let is_a_y (s:string) = s.StartsWith("y")
    let is_a_z (s:string) = s.StartsWith("z")

    let xorOps = ops |> Seq.filter( fun (l,op,r,out) -> op = "XOR") |> Seq.toArray
    let andOps = ops |> Seq.filter( fun (l,op,r,out) -> op = "AND") |> Seq.toArray
    let isXYop i (l:string,op:string,r:string,_:string) = (l |> isX i && r |> isY i) || (r |> isX i && l |> isY i)
    let is_an_XY_op (l:string,op:string,r:string,_:string) = (l |> is_a_x && r |> is_a_y) || (r |> is_a_x && l |> is_a_y)

    let fails = new List<string>()

    for i =1 to 44 do
        match andOps |> Seq.tryFind(fun (l,op,r,out) -> isXYop i (l,op,r,out) && (is_a_z out)) with
        | Some (l,op,r,out) ->
            printfn "Found %s for %s %s with output is %s which is z and should not" op l r out
            fails.Add(out)
        | _ -> ()

    for (l, op, r, out) in ops do
        if is_a_x l && is_a_y r then
            if op = "OR" then
                printfn "invalid op %A" (l, op, r, out)
                fails.Add(out)
            if op = "XOR" && is_a_z out then
                if isXYop 0 (l, op, r, out) = false then
                    printfn "x XOR y into a z %A" (l, op, r, out)
                    fails.Add(out)
            if op = "AND" && is_a_z out then
                printfn "x AND y into a z %A" (l, op, r, out)
                fails.Add(out)

    for (l, op, r, out) in ops do
        if is_a_z out then
            if op <> "XOR" && (out <> "z45") then
                printfn "Gate ouputs z but is not an XOR %A" (l, op, r, out)
                fails.Add(out)

    xorOps 
    |> Seq.filter (fun op -> not(is_an_XY_op op))
    |> Seq.iter (fun (l,op,r,out) ->
        if not (is_a_z out) then
            printfn "XOR that is not a x XOR y doesn't output in z %A" (l,op,r,out)
            fails.Add(out)
    )

    xorOps
    |> Seq.iter (fun (l,op,r,out) ->
        ops |> Seq.iter (fun (sl,sop,sr,sout) ->
            if (out = sl || out = sr) && (sop = "OR") then
                printfn "XOR output should be an AND %A" (l,op,r,out)
                fails.Add(out)
        )
    )

    ops 
    |> Seq.iter (fun (l,op,r,out) ->
        if op = "AND" && (l <> "x00") && (r <> "x00") then
            ops |> Seq.iter (fun (sl,sop,sr,sout) ->
                if (out = sl || out = sr) && (sop <> "OR") then
                    printfn "AND output should be an OR %A" (l,op,r,out)
                    fails.Add(out)
            )
    )

    String.Join(",", fails |> Seq.distinct |> Seq.sort |> Seq.toArray)

solve2 "Day22.txt"

//#r "nuget: Quikgraph"
//#r "nuget: Quikgraph.GraphViz"

//open QuikGraph
//open QuikGraph.Graphviz.Dot
//open QuikGraph.Graphviz

//let solve2 input =
//    let vars, ops = getInput input

//    let g = new QuikGraph.EdgeListGraph<string, TaggedEdge<string, string>>()

//    for (left,op,right,output) in ops do
//        g.AddEdge(new TaggedEdge<string, string>(left, output, op)) |> ignore
//        g.AddEdge(new TaggedEdge<string, string>(right, output, op))  |> ignore
    
//    let dotGraph = g.ToGraphviz(fun algo ->
//        algo.CommonVertexFormat.Shape <- GraphvizVertexShape.Diamond
//        algo.FormatEdge.Add(fun args -> args.EdgeFormat.Label.Value <- $"{args.Edge.Tag}")
//        algo.FormatVertex.Add(fun args -> args.VertexFormat.Label <- $"{args.Vertex}")
//    )
//    File.WriteAllText(@"c:\temp\aoc2024\24.dot", dotGraph)
//    // Visualizing this graph make it pretty clear there is structure
//    // looking around a bit it looks like a https://en.wikipedia.org/wiki/Adder_(electronics)#Ripple-carry_adder
