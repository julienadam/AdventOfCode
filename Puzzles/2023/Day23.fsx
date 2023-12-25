#time "on"
#load "../../Tools.fs"
#load "../../Tools/Array2DTools.fs"
#r "nuget: FSharp.Collections.ParallelSeq"
#r "nuget: NFluent"

open System.IO
open AdventOfCode
open AdventOfCode.Array2DTools
open FSharp.Collections.ParallelSeq
open NFluent
open System.Collections.Generic

let getInput name = 
    let grid = 
        File.ReadAllLines(getInputPath2023 name)
        |> Array.map (fun line -> line.ToCharArray())
        |> array2D
    // Plug the entrance and exit to avoid bound checks
    grid[0,1] <- '#'
    grid[(grid |> maxR), ((grid |> maxC) - 1)] <- '#'
    grid

let getAdjacentWithSlopes (grid:char array2d) r c =
    match grid[r,c] with
    | '>' -> [ r, c + 1 ]
    | '<' -> [ r, c - 1 ]
    | 'v' -> [ r + 1, c ]
    | '^' -> [ r - 1, c ]
    | '.' -> [ r - 1, c; r + 1, c; r, c - 1; r, c + 1 ]
    | _ -> failwith "error"
    
let solve getAdjacent input =
    let grid = getInput input 
    let target = (grid |> maxR) - 1, (grid |> maxC) - 1

    let rec walk r c path =
        let p = path |> Set.add (r,c)
        // printfn "%i,%i" r c
        if (r,c) = target then
            p
        else
            let adjacent = 
                getAdjacent grid r c
                |> List.filter (fun (ar, ac) -> grid[ar,ac] <> '#' && p.Contains(ar,ac) = false)

            if adjacent.IsEmpty then 
                    Set.empty
                else
                    adjacent 
                    |> List.map(fun (ar,ac) -> walk ar ac p)
                    |> List.maxBy(fun p -> p.Count)

    let longestPath = walk 1 1 Set.empty
    longestPath.Count + 1

let solve1 = solve getAdjacentWithSlopes

// Check.That(solve1 "Day23_sample1.txt").IsEqualTo(94)
// solve1 "Day23.txt"

let getAdjacentWithoutSlopes (grid:char array2d) r c =
    match grid[r,c] with
    | '>'
    | '<'
    | 'v'
    | '^'
    | '.' -> [ r - 1, c; r + 1, c; r, c - 1; r, c + 1 ]
    | x -> failwithf "found %c at %i,%i" x r c

#r "nuget: QuikGraph"
#r "nuget: QuikGraph.GraphViz"

open QuikGraph
open QuikGraph.Algorithms

let solve2 name = 

    let grid = getInput name

    let buildGraph grid getAdjacent = 
        let graph = new AdjacencyGraph<(int*int), TaggedEdge<(int*int), int>>();
        let visited = new HashSet<int*int>()
        let toWalk = new Queue<(int*int)*(int*int)*int>()
        let target = (grid |> maxR) - 1, (grid |> maxC) - 1

        // let ensureEdge (r,c) =
        //     if graph.ContainsVertex(r,c) = false then
        //         graph.AddVertex(r,c) |> ignore

        let rec walk () =
            match toWalk.TryDequeue() with
            | true, (n, s, l) when n = target-> 
                printfn "Found exit"
                graph.AddVertex(s) |> ignore
                graph.AddVertex(n) |> ignore
                graph.AddEdge(new TaggedEdge<(int*int), int>(s, n, l + 1)) |> ignore
                ()
            | true, ((r,c), (sr,sc), l) -> 
                visited.Add(r,c) |> ignore
                let adjacent = 
                    getAdjacent grid r c
                    |> List.filter (fun (ar, ac) -> grid[ar,ac] <> '#')
                match adjacent with
                | [a;b] when visited.Contains(a) ->
                    toWalk.Enqueue(b, (sr,sc), l + 1) |> ignore
                | [a;b] when visited.Contains(b) ->
                    toWalk.Enqueue(a, (sr,sc), l + 1) |> ignore
                | [] -> 
                    ()
                | next ->
                    printfn "Adding edge from (%i,%i) to (%i,%i) with length %i" sr sc r c (l+1)
                    graph.AddVertex((sr,sc)) |> ignore
                    graph.AddVertex((r,c)) |> ignore
                    graph.AddEdge(new TaggedEdge<(int*int), int>((sr,sc), (r,c), l + 1)) |> ignore
                    next 
                    |> List.filter (fun n -> visited.Contains(n) = false)
                    |> List.iter (fun n -> toWalk.Enqueue(n, (r,c), 1))

                walk()
            | false, _ ->
                printfn "List is empty"
                ()

        toWalk.Enqueue((1,1), (1,1), 0)
        walk ()

        graph


    let graph = buildGraph grid getAdjacentWithoutSlopes
    let graphViz = new Graphviz.GraphvizAlgorithm<(int*int), TaggedEdge<(int*int), int>>(graph)
    let dotGraph = graphViz.Generate()
    File.WriteAllText(@"f:\temp\23.dot", dotGraph)
    graph

solve2 "Day23_sample1.txt"

    // let reduced  = graph.ComputeTransitiveClosure()
    
    
    

    