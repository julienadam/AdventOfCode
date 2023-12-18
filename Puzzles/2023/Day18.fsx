#time "on"
#load "../../Tools.fs"
#load "../../Tools/Directions.fs"
#load "../../Tools/SparseGrid.fs"
#r "nuget:NFluent"

open System.IO
open System.Drawing
open AdventOfCode
open System.Collections.Generic
open SparseGrid
open NFluent

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

let digTrench (instructions:(Direction * int * Color) seq) =
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

let digInterior (grid:IDictionary<(int*int), unit>) =
    let minR = grid |> minR
    let maxR = grid |> maxR
    let minC = grid |> minC
    let maxC = grid |> maxC
    for r = minR to maxR do
        let mutable enteredDir = None
        let mutable walls = 0
        for c = minC to maxC do
            let isEdge = grid.ContainsKey(r,c)
            match isEdge, enteredDir with
            | false, _ -> 
                enteredDir <- None
                if walls % 2 = 1 then
                    grid.Add((r,c), ()) |> ignore
            | true, None ->
                match grid.ContainsKey(r-1,c), grid.ContainsKey(r+1,c) with
                | true, true -> walls <- walls + 1
                | false, false -> failwithf "Single cell edge not supported at %i,%i" r c
                | true, false -> enteredDir <- Some North
                | false, true -> enteredDir <- Some South
            | true, Some dir  ->
                match dir, grid.ContainsKey(r-1,c), grid.ContainsKey(r+1,c) with
                | _, true, true
                | North, false, true
                | South, true, false -> walls <- walls + 1
                | _, false, false
                | North, true, false
                | South, false, true -> ()
                | x -> failwithf "Unexpected interior configuration %A at %i,%i" x r c
    grid

let solve1 input =
    let digSite = 
        getInput input 
        |> digTrench
    
    // let text = gridToText (fun x -> match x with | Some _ -> '█' | None -> '.') digSite
    // File.WriteAllText(@"F:\temp\d18_edges.txt", text)

    let digSite = 
        digSite
        |> digInterior
        
    // let text = gridToText (fun x -> match x with | Some _ -> '█' | None -> '.') digSite
    // File.WriteAllText(@"F:\temp\d18_filled.txt", text)

    digSite.Count

Check.That(solve1 "Day18_sample1.txt").IsEqualTo(62)

solve1 "Day18.txt"
