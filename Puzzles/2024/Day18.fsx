#time "on"
#load "../../Tools.fs"
#load "../../Tools/Array2DTools.fs"
#load "../../Tools/AStar.fs"
#load "../../Tools/Distance.fs"
#r "nuget: NFluent"

open System
open System.IO
open AdventOfCode
open Checked
open Array2DTools
open FullAStar
open NFluent

let getInput name = 
    File.ReadAllLines(getInputPath2024 name)
    |> Seq.map (splitIntList >> tupleize2)

let cost a b = 1.0
let getNeighbors (blockMap:Set<int*int>) (height,width) (r,c) = 
    Array2DTools.getAdjacentCoords r c width height
    |> Seq.filter (fun (r,c) ->
        blockMap.Contains((r,c)) |> not
    )

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
    let fallenBlocks = fallingBlocks |> Seq.take nanoseconds |> Set.ofSeq
    match tryFindPath height width fallenBlocks with
    | Some path -> (path |> Seq.length) - 1
    | _ -> failwithf "No path found"

Check.That(solve1 "Day18_sample1.txt" 12).Equals(22)
solve1 "Day18.txt" 1024

//let grid = Array2D.create height width '.'
   //fallingBlocks |> Seq.take nanoseconds |> Seq.iter (fun (r,c) ->
   //    grid[r,c] <- '#'
   //)
   //printGrid grid