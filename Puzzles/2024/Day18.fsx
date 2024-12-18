#time "on"
#load "../../Tools.fs"
#load "../../Tools/Array2DTools.fs"
#load "../../Tools/AStar.fs"
#load "../../Tools/Distance.fs"
#r "nuget: NFluent"

open System.IO
open System.Collections.Generic
open AdventOfCode
open Checked
open FullAStar
open NFluent

let getInput name = 
    File.ReadAllLines(getInputPath2024 name)
    |> Seq.map (splitIntList >> tupleize2)

let cost _ _ = 1.0
let getNeighbors (blockMap:HashSet<int*int>) (height,width) (r,c) = 
    Array2DTools.getAdjacentCoords r c width height
    |> Seq.filter (fun p -> blockMap.Contains(p) |> not)

let tryFindPath height width fallenBlocks =
    let config :Config<int*int> = {
        gCost = cost
        fCost = fun a b -> manhattanDistPoints a b |> float
        neighbors = (getNeighbors fallenBlocks (height, width))
        maxIterations = None
    }
    FullAStar.search (0,0) (height - 1, width - 1) config

let solve1 input nanoseconds =
    let fallingBlocks = getInput input
    let height = (fallingBlocks |> Seq.map fst |> Seq.max) + 1
    let width = (fallingBlocks |> Seq.map snd |> Seq.max) + 1
    let fallenBlocks = new HashSet<int*int>(fallingBlocks |> Seq.take nanoseconds)
    match tryFindPath height width fallenBlocks with
    | Some path -> (path |> Seq.length) - 1
    | _ -> failwithf "No path found"

Check.That(solve1 "Day18_sample1.txt" 12).Equals(22)
solve1 "Day18.txt" 1024

// Part 2 - Third attempt. Start with a full map, remove fallen blocks until a path is found
let solve2c input =
    let fallingBlocks = getInput input
    let height = (fallingBlocks |> Seq.map fst |> Seq.max) + 1
    let width = (fallingBlocks |> Seq.map snd |> Seq.max) + 1

    let mutable fallenBlocks = new HashSet<int*int>(fallingBlocks)
    
    let (br,bc) =
        fallingBlocks |> Seq.rev |> Seq.find (fun b ->
            fallenBlocks.Remove(b) |> ignore
            match tryFindPath height width fallenBlocks with
            | Some _ -> 
                printfn "Found clear path after %i blocks" (fallenBlocks.Count)
                true
            | _ ->  false
            
        )
    sprintf "%i,%i" br bc

Check.That(solve2c "Day18_sample1.txt").Equals("6,1")
solve2c "Day18.txt" // Takes about 30ms

// Part 2 - First attempt, just A* each "nanosecond" with one more block
let solve2 input =
    let fallingBlocks = getInput input
    let height = (fallingBlocks |> Seq.map fst |> Seq.max) + 1
    let width = (fallingBlocks |> Seq.map snd |> Seq.max) + 1

    let mutable fallenBlocks = new HashSet<int*int>()
    let (br,bc) = 
        fallingBlocks |> Seq.find (fun b ->
            if fallenBlocks.Count % 100 = 0 then printf "."
            fallenBlocks.Add(b) |> ignore
            match tryFindPath height width fallenBlocks with
            | Some _ -> false
            | _ -> true
        )
    sprintf "%i,%i" br bc

Check.That(solve2 "Day18_sample1.txt").Equals("6,1")
solve2 "Day18.txt" // Takes about a minute or two


// Part 2 - Second attempt. A* the initial map. This gives us the current best path. 
// If a block falls anywhere outside the best path, ignore it. 
// If not, compute the new best past. Rinse and repeat until we are blocked
// This runs in under a second on my machine compared to about 2 minutes for my first attempt below
let solve2b input =
    let fallingBlocks = getInput input
    let height = (fallingBlocks |> Seq.map fst |> Seq.max) + 1
    let width = (fallingBlocks |> Seq.map snd |> Seq.max) + 1

    let mutable fallenBlocks = new HashSet<int*int>()
    let mutable bestPathBlocks = (tryFindPath height width fallenBlocks).Value |> Set.ofSeq

    let (br,bc) =
        fallingBlocks |> Seq.find (fun b ->
            fallenBlocks.Add(b) |> ignore
            if bestPathBlocks.Contains b then
                match tryFindPath height width fallenBlocks with
                | Some path -> 
                    bestPathBlocks <- path |> Set.ofSeq
                    false
                | _ -> 
                    printfn "Found blocked path after %i blocks" (fallenBlocks.Count)
                    true
            else
                false
        )
    sprintf "%i,%i" br bc

Check.That(solve2b "Day18_sample1.txt").Equals("6,1")
solve2b "Day18.txt" // Takes about a second
