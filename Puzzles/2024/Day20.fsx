#time "on"
#r "nuget: optimizedpriorityqueue"
#load "../../Tools.fs"
#load "../../Tools/Array2DTools.fs"
#load "../../Tools/Dijkstra.fs"
#load "../../Tools/Distance.fs"

open System
open System.IO
open AdventOfCode
open Array2DTools
open Checked
let getInput name = File.ReadAllLines(getInputPath2024 name) |> array2D

let solveDij maxPicoSecondsAllowed input cutOff = 
    let grid = getInput input 
    let (er,ec, _) = grid |> Array2DTools.findi (fun _ _ v -> v = 'E')
    let vertices = grid |> filteri (fun _ _ v -> v <> '#') |> Seq.map (fun (r,c,_) -> (r,c)) |> Seq.toList
    let getNeighbors (r,c) = grid |> Array2DTools.getAdjacent r c |> Seq.filter (fun (_,_,v) -> v <> '#') |> Seq.map (fun (r,c,_) -> (r,c)) |> Seq.toList

    let distMatrix = Dijkstra.getDistMatrix (er,ec) vertices getNeighbors (fun a b -> 1.0f)

    let matrix = distMatrix |> Seq.map (fun kvp -> kvp.Key, kvp.Value) |> Seq.toList
    Seq.allPairs matrix matrix 
    |> Seq.filter (fun ((a,d1), (b,d2)) -> 
        let md = (manhattanDistPoints a b |> float32)
        d2 - d1 - md >= cutOff && md <= maxPicoSecondsAllowed)
    |> Seq.length


let solve1 = solveDij 2f
// solve1 "Day20_sample1.txt" 12f
solve1 "Day20.txt" 100f

let solve2 = solveDij 20f
// solveDij "Day20_sample1.txt" 50f 20f
solve2 "Day20.txt" 100f 