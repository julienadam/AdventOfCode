#time "on"
#load "../../Tools.fs"
#load "../../Tools/Array2DTools.fs"

open System
open System.IO
open AdventOfCode
open Array2DTools

let ctoi (c:char) = (c |> int) - ('0' |> int)

let getInput name = File.ReadAllLines(getInputPath2024 name) |> array2D |> Array2D.map ctoi

let score (grid:int[,]) (startR, startC) =
    let rec scoreRec (r,c) : (int*int)seq=
        let height = grid[r,c]
        match height with
        | 9 -> [(r,c)]
        | _ ->
            grid 
            |> Array2DTools.getAdjacent r c
            |> Seq.filter(fun (_, _, v) -> v = height + 1)
            |> Seq.map (fun (ar,ac, _) -> scoreRec (ar,ac))
            |> Seq.concat

    scoreRec (startR,startC)

let solve1 input =
    let grid = getInput input
    grid
    |> enumArray2d
    |> Seq.filter(fun (r,c,v) -> v = 0) // find trail heads
    |> Seq.map (fun (r,c,_) -> (r,c)) // get coords only
    |> Seq.collect (score grid >> Seq.distinct) // find all trail ends for each trail head and collect them
    |> Seq.length 

solve1 "Day10.txt"

let solve2 input =
    let grid = getInput input
    grid
    |> enumArray2d
    |> Seq.filter(fun (r,c,v) -> v = 0) // find trail heads
    |> Seq.map (fun (r,c,_) -> (r,c)) // get coords only
    |> Seq.map (score grid >> Seq.length) // find all trail ends for each trail head and collect them
    |> Seq.sum 

solve2 "Day10.txt"