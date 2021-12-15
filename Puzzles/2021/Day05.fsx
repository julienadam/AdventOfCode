#load "../../Tools.fsx"

open System
open System.Collections.Generic
open System.IO
open System.Text.RegularExpressions
open Tools

let path = getInputPath "day05.txt"
//let path = getInputPath "day05_sample1.txt"

let reLine = Regex("(?<x1>\d+),(?<y1>\d+) -> (?<x2>\d+),(?<y2>\d+)")

type Segment = {
    Start: int * int
    End: int * int
}

let mapLine l = 
    let m = reLine.Match l
    if m.Success then
        { Start = (m.Groups.["x1"].Value |> int, m.Groups.["y1"].Value |> int); End = (m.Groups.["x2"].Value |> int, m.Groups.["y2"].Value |> int) }
    else
        failwithf "invalid line read %s" l

let fileLines = File.ReadAllLines(path) |> Array.map mapLine

let printGrid (grid:Dictionary<int*int, int>) =
    for i = 0 to 9 do
        for j = 0 to 9 do
            match grid.TryGetValue((j, i)) with
            | true, value -> printf "%i" value
            | _ -> printf "."
        printfn ""

let updateGridCell (grid:Dictionary<int * int, int>) x y =
    match grid.TryGetValue((x, y)) with
    | true, current -> grid.[(x, y)] <- current + 1
    | false, _ -> grid.Add((x, y), 1)

module Part1 =
    
    let Solve() = 
        let updateGrid (grid:Dictionary<int*int, int>) segment =
            match segment.Start, segment.End with
            | (x1,y1), (x2,y2) when x1 = x2 -> 
                for y = (min y1 y2) to (max y1 y2) do
                    updateGridCell grid x1 y
                grid
            | (x1,y1), (x2,y2) when y1 = y2 ->
                for x = (min x1 x2) to (max x1 x2) do
                    updateGridCell grid x y1
                grid
            | _ ->
                grid
                
        let finalGrid = fileLines |> Array.fold updateGrid (new Dictionary<int*int, int>())
        let gridValues = finalGrid.Values |> Dump
        gridValues |> Seq.where (fun s -> s > 1) |> Seq.length |> Dump
        ()

Part1.Solve()

module Part2 =
    let Solve() = 
        let updateGrid (grid:Dictionary<int*int, int>) segment =
            match segment.Start, segment.End with
            | (x1,y1), (x2,y2) when x1 = x2 -> 
                for y = (min y1 y2) to (max y1 y2) do
                    updateGridCell grid x1 y
                grid
            | (x1,y1), (x2,y2) when y1 = y2 ->
                for x = (min x1 x2) to (max x1 x2) do
                    updateGridCell grid x y1
                grid
            // Diagonals
            | (x1,y1), (x2,y2) -> 
                updateGridCell grid x2 y2
                let mutable sx = x1
                let mutable sy = y1
                while (sx, sy) <> (x2,y2) do
                    updateGridCell grid sx sy
                    sx <- if x1 < x2 then sx + 1 else sx - 1
                    sy <- if y1 < y2 then sy + 1 else sy - 1
                grid
            | _ ->
                failwithf "Should not happen"
                grid
                
        let finalGrid = fileLines |> Array.fold updateGrid (new Dictionary<int*int, int>())
        finalGrid |> printGrid
        let gridValues = finalGrid.Values |> Dump
        gridValues |> Seq.where (fun s -> s > 1) |> Seq.length |> Dump
        ()
        
Part2.Solve()