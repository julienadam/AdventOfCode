#time "on"
#load "../../Tools.fs"
#r "nuget: NFluent"

open System
open System.IO
open AdventOfCode
open NFluent

let getInput name = 
    File.ReadAllLines(getInputPath2023 name)
    |> Array.collect (fun line ->
        let src,targets = line |> ssplit2 ": "
        let targets = targets |> ssplit " "
        targets |> Array.map (fun t -> src,t)
    )

type Edge = { src : int;  dest : int }

type Graph = {
        V: int
        edges : Edge array
} with
    member this.E = this.edges.Length

type Subset = { mutable parent: int; mutable rank: int }

// A utility function to find set of an element i using path compression technique
let rec find (subsets:Subset array) i = 
    // find root and make root as parent of i
    if subsets[i].parent <> i then
        subsets[i].parent <- find subsets subsets[i].parent

    subsets[i].parent

// Union of two sets by rank
let union (subsets:Subset[]) x y =
    let xRoot = find subsets x
    let yRoot = find subsets y

    // Attach smaller rank tree under root of high rank tree
    if subsets[xRoot].rank < subsets[yRoot].rank then
        subsets[xRoot].parent <- yRoot
    else if subsets[xRoot].rank > subsets[yRoot].rank then
        subsets[yRoot].parent <- xRoot
    else
        // If ranks are same, then make one as root and increment its rank by one
        subsets[yRoot].parent <- xRoot
        subsets[xRoot].rank <- subsets[xRoot].rank + 1

let kargerMinCut graph =
    let rnd = new Random();
    // Get data of given graph
    let V = graph.V
    let E = graph.E
    let edges = graph.edges

    // Initialize subsets
    let subsets = [|0..V-1|] |> Array.map (fun v -> { parent = v; rank = 0 })

    // Initially there are V vertices in
    // contracted graph
    let mutable vertices = V

    // Keep contracting vertices until there are
    // 2 vertices.
    while (vertices > 2) do
        // Pick a random edge
        let i = rnd.Next() % E

        // Find vertices of two corners of current edge
        let subset1 = find subsets edges[i].src
        let subset2 = find subsets edges[i].dest

        if (subset1 <> subset2) then
            // Contract the edge (or combine the corners of edge into one vertex)
            vertices <- vertices - 1
            union subsets subset1 subset2

    // Now we have two vertices (or subsets) left in
    // the contracted graph, so count the edges between
    // two components and return the count.
    let mutable cutEdges = 0
    let subsetsFound = new System.Collections.Generic.List<int*int>()
    
    for i = 0 to E - 1 do
        let subset1 = find subsets edges[i].src
        let subset2 = find subsets edges[i].dest
        if (subset1 <> subset2) then
            subsetsFound.Add(subset1,subset2)
            cutEdges <- cutEdges+1
    
    (cutEdges, subsets, subsetsFound)

let makeGraph (edges:('a*'a) array) =
    let vertices = 
        edges 
        |> Array.collect (fun (a,b) -> [|a;b|])
        |> Array.distinct

    let indexedEdges = 
        edges 
        |> Array.map (fun (l,r) -> { src = Array.IndexOf(vertices, l); dest = Array.IndexOf(vertices, r) })

    { V = vertices.Length; edges = indexedEdges }

let solve1 name =
    let edges = getInput name
    let graph = makeGraph edges
    
    let rec loop() = 
        let (cuts, subsets, found) = kargerMinCut graph
        if cuts = 3 then
            match found |> Seq.collect (fun (a,b) -> [a;b]) |> Seq.distinct |> Seq.toList with
            | [left;right] ->
                let sizeA = subsets |> Seq.where(fun s -> s.parent = left) |> Seq.length
                let sizeB = subsets |> Seq.where(fun s -> s.parent = right) |> Seq.length
                (sizeA * sizeB)
            | _ -> failwith "More than 2 sets !"
            
        else
            loop()
        
    loop()

Check.That(solve1 "Day25_sample1.txt").IsEqualTo(54)
solve1 "Day25.txt"
