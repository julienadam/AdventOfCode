#time "on"
#load "../../Tools.fs"
#load "../../Tools/Array2DTools.fs"

open System
open System.IO
open AdventOfCode

let getInput name = 
    let grid = File.ReadAllLines(getInputPath2024 name) |> array2D
    let antennas = 
        grid
        |> Array2DTools.enumArray2d
        |> Seq.choose (fun (r,c,v) -> match v with | '.' -> None | x -> Some (r,c,x)) // keep only antenna cells
        |> Seq.groupBy (fun (_,_,power) -> power) // group by power
        |> Seq.map (fun (power,antennas) -> power, antennas |> Seq.map (fun (r,c,_) -> (r,c)) |> Seq.toArray) // index by power, store (r,c) coords
    grid, antennas


let findAntinodes grid (r1,c1) (r2,c2) =
    let dr = r2-r1
    let dc = c2-c1
    [(r1-dr, c1-dc); (r2+dr, c2+dc)] 
    |> Seq.filter (fun (r,c) -> grid |> Array2DTools.isInBounds r c)

let applyAntiNodeFinder finder antennas =
    antennas 
    |> Seq.collect (fun (_, antennas) ->
        Seq.allPairs antennas antennas // get all pairs of antennas of the same power
        |> Seq.filter (fun (p1,p2) -> p1 <> p2) // remove self pairs
        |> Seq.collect (fun (p1,p2) -> finder p1 p2) // find their antinodes
    )
    |> Seq.distinct

let solve1 input =
    let grid, antennas = getInput input
    antennas |> applyAntiNodeFinder (findAntinodes grid) |> Seq.length

solve1 "Day08.txt"

let findResonantHarmonicAntinodes grid (r1,c1) (r2,c2) =
    let dr = r2-r1
    let dc = c2-c1
    let mutable mr1 = r1
    let mutable mc1 = c1
    let mutable mr2 = r2
    let mutable mc2 = c2
    seq {
        // walk in one direction until out of bounds
        while(grid |> Array2DTools.isInBounds mr1 mc1) do
            yield (mr1,mc1)
            mr1 <- mr1-dr
            mc1 <- mc1-dc
        // and in the other direction
        while(grid |> Array2DTools.isInBounds mr2 mc2) do
            yield (mr2,mc2)
            mr2 <- mr2-dr
            mc2 <- mc2-dc
    }

let solve2 input =
    let grid, antennas = getInput input
    antennas |> applyAntiNodeFinder (findResonantHarmonicAntinodes grid) |> Seq.length

solve2 "Day08.txt"