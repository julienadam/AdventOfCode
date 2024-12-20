#time "on"
#load "../../Tools.fs"
#load "../../Tools/Array2DTools.fs"
#load "../../Tools/AStar.fs"
#load "../../Tools/Distance.fs"

open System
open System.IO
open AdventOfCode
open Checked

let getInput name = File.ReadAllLines(getInputPath2024 name) |> array2D

let solve1 input =
    let grid = getInput input 
    let (sr,sc, _) = grid |> Array2DTools.findi (fun _ _ v -> v = 'S')
    let (er,ec, _) = grid |> Array2DTools.findi (fun _ _ v -> v = 'E')
    
    let config:FullAStar.Config<(int*int)> = {
        gCost = (fun _ _ -> 1.0)
        fCost = (fun a b -> manhattanDistPoints a b |> float)
        neighbors = (fun (r,c) -> grid |> Array2DTools.getAdjacent r c |> Seq.filter (fun (_,_,v) -> v <> '#') |> Seq.map (fun (r,c,_) -> (r,c)))
        maxIterations = None
    }

    match FullAStar.search (sr,sc) (er,ec) config with
    | Some p -> printfn "Best path is %i long" ((p |> Seq.length) - 1)
    | None -> printfn "No path found"

solve1 "Day20_sample1.txt"
