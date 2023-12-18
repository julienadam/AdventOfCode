#time "on"
#load "../../Tools.fs"
#load "../../Tools/Directions.fs"
#load "../../Tools/SparseGrid.fs"

open System
open System.IO
open System.Drawing
open AdventOfCode
open System.Collections.Generic
open SparseGrid
open Directions

let parseLine line =
    let split = line |> ssplit " "
    let dir = 
        match split[0] with 
        | "U" -> North
        | "D" -> South
        | "L" -> West
        | "R" -> East
        | _ -> failwithf "invalid dir"
    let length = split[1] |> int
    let color = Color.FromName(split[2].Substring(2,6))
    dir, length, color

let getInput name =
    File.ReadAllLines(getInputPath2023 name)
    |> Array.map parseLine

let dig (instructions:(Direction * int * Color) seq) =
    let grid = new Dictionary<(int*int), unit>()
    instructions 
    |> Seq.fold(fun (row, col) (dir, len, _) ->
        [1..len] |> Seq.fold(fun (r,c) _ -> 
            let newPos = dir.Move (r, c)
            grid.Add(newPos, ()) |> ignore
            newPos
        ) (row, col)
    ) (0,0)
    |> ignore

    grid

let solve1 input =
    getInput input 
    |> dig
    |> printGrid (fun x -> match x with | Some _ -> '█' | None -> '.')
    |> ignore

solve1 "Day18_sample1.txt"

