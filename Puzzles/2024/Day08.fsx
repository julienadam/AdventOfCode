#time "on"
#load "../../Tools.fs"
#load "../../Tools/Array2DTools.fs"

open System
open System.IO
open AdventOfCode

let getInput name = 
    File.ReadAllLines(getInputPath2024 name)
    |> array2D

let findAntinodes (r1,c1) (r2,c2) =
    let dr = r2-r1
    let dc = c2-c1
    [(r1-dr, c1-dc); (r2+dr, c2+dc)]

let solve1 input =
    let grid = getInput input
    let antennas = 
        grid
        |> Array2DTools.enumArray2d
        |> Seq.choose (fun (r,c,v) -> match v with | '.' -> None | x -> Some (r,c,x)) // keep only antenna cells
        |> Seq.groupBy (fun (_,_,power) -> power) // group by power
        |> Seq.map (fun (power,antennas) -> power, antennas |> Seq.map (fun (r,c,_) -> (r,c)) |> Seq.toArray) // index by power, store (r,c) coords

    antennas 
    |> Seq.collect (fun (_, antennas) ->
        Seq.allPairs antennas antennas // get all pairs of antennas of the same power
        |> Seq.filter (fun (p1,p2) -> p1 <> p2) // remove self pairs
        |> Seq.collect (fun (p1,p2) -> findAntinodes p1 p2) // find their antinodes
        |> Seq.filter (fun (r,c) -> grid |> Array2DTools.isInBounds r c) // filter out of bounds antinodes
    )
    |> Seq.distinct // |> Seq.toArray |> Dump
    |> Seq.length

solve1 "Day08.txt"
