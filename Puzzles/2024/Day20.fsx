
#time "on"
#r "nuget: optimizedpriorityqueue"
#load "../../Tools.fs"
#load "../../Tools/Array2DTools.fs"
#load "../../Tools/Dijkstra.fs"
#load "../../Tools/Distance.fs"
#r "nuget: FSharp.Collections.ParallelSeq"

open System.IO
open AdventOfCode
open Array2DTools
open Checked
open FSharp.Collections.ParallelSeq

let getInput name = File.ReadAllLines(getInputPath2024 name) |> array2D

let solveDij maxPicoSecondsAllowed input cutOff = 
    let grid = getInput input 
    let one _ _ = 1f
    let (er,ec, _) = grid |> Array2DTools.findi (fun _ _ v -> v = 'E')
    let vertices = grid |> filteri (fun _ _ v -> v <> '#') |> Seq.map (fun (r,c,_) -> (r,c)) |> Seq.toList
    let getNeighbors (r,c) = grid |> Array2DTools.getAdjacent r c |> Seq.filter (fun (_,_,v) -> v <> '#') |> Seq.map (fun (r,c,_) -> (r,c)) |> Seq.toList

    let distMatrix = Dijkstra.getDistMatrix (er,ec) vertices getNeighbors one
    let sortedByDist = distMatrix |> Seq.map (fun kvp -> kvp.Key, kvp.Value) |> Seq.sortBy snd |> Seq.toArray

    sortedByDist
    |> PSeq.mapi (fun i (a,d1) ->
        sortedByDist 
        |> Array.skip i 
        |> Seq.filter (fun (b,d2) ->
            let md = (manhattanDistPoints a b |> float32)
            d2 - d1 - md >= cutOff && md <= maxPicoSecondsAllowed)
        |> Seq.length
    )
    |> Seq.sum

let solve1 = solveDij 2f
// solve1 "Day20_sample1.txt" 12f
solve1 "Day20.txt" 100f

let solve2 = solveDij 20f
// solveDij "Day20_sample1.txt" 50f 20f
solve2 "Day20.txt" 100f 