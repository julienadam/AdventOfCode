

#load "../../Tools.fsx"

open System
open System.Collections.Generic
open System.Diagnostics
open System.IO
open System.Text.RegularExpressions
open Tools

type Range = int*int
type Cuboid = Range * Range * Range
type RebootInstruction = Cuboid * bool

let mapLine line = 
    let regex = new Regex("(?<state>on|off) x=(?<xmin>-?\d+)\.\.(?<xmax>-?\d+),y=(?<ymin>-?\d+)\.\.(?<ymax>-?\d+),z=(?<zmin>-?\d+)\.\.(?<zmax>-?\d+)")
    let m = regex.Match(line)
    let active = match m.Groups.["state"].Value with | "on" -> true | "off" -> false | _ -> failwithf "invalid pixel state"
    let xRange = m |> mInt "xmin", m |> mInt "xmax"
    let yRange = m |> mInt "ymin", m |> mInt "ymax"
    let zRange = m |> mInt "zmin", m |> mInt "zmax"
    let (instruction:RebootInstruction) = (xRange, yRange, zRange), active
    instruction

// mapLine "off x=-54112..-39298,y=-85059..-49293,z=-27449..7877"

let enumRange (s,e) = seq { 
    for i = s to e do yield i
}

let solve1 fileName = 
    let input = getInputPath fileName |> File.ReadAllLines |> Seq.map mapLine
    
    let filterInitializationCubes (instructions:RebootInstruction seq) =
        let inline insideInitBounds (start, endp) = start >= -50 && endp <= 50
        instructions |> Seq.filter (fun ((xr, yr, zr), _) ->
            insideInitBounds xr && insideInitBounds yr && insideInitBounds zr
        )

    let filteredInput = input |> filterInitializationCubes |> Seq.toList

    let grid = new Dictionary<(int * int * int), bool>()
    
    filteredInput 
    |> Seq.iter(fun ((xr,yr,zr), state) -> 
        for x in enumRange xr do
            for y in enumRange yr do
                for z in enumRange zr do
                    grid.[(x,y,z)] <- state
    )

    grid.Values |> Seq.filter id |> Seq.length


assert(solve1 "Day22_Sample1.txt" = 39)
assert(solve1 "Day22_Sample2.txt" = 590784)
let sw = Stopwatch.StartNew()
let result = solve1 "Day22.txt"
printfn "Day 22 Part 1 solution : %i. Took %A" result sw.Elapsed


let solve2 fileName = 
    let input = getInputPath fileName |> File.ReadAllLines |> Seq.map mapLine |> Seq.toList
    let totalInstructions = input.Length
    let grid = new Dictionary<(int * int * int), bool>()
    
    input
    |> Seq.iteri(fun i ((xr,yr,zr), state) -> 
        printfn "Iteration %i of %i" i totalInstructions
        for x in enumRange xr do
            for y in enumRange yr do
                for z in enumRange zr do
                    grid.[(x,y,z)] <- state
    )

    grid.Values |> Seq.filter id |> Seq.length

sw.Restart()
let r2 = solve2 "Day22_sample3.txt"
printfn "Day 22 Part 2 sample  : %i. Took %A" r2 sw.Elapsed
// TODO : filter again for init cubes after reboot proc. Shouldn't be necessary as the coords don't change