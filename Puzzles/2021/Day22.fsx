#load "../../Tools.fs"

open System
open System.Collections.Generic
open System.Diagnostics
open System.IO
open System.Text.RegularExpressions
open AdventOfCode

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

let enumRange (s,e) = seq { 
    for i = s to e do yield i
}

let computeOnCubesWithBruteForce instructions = 

    let filterInitializationCubes (instructions:RebootInstruction seq) =
        let inline insideInitBounds (start, endp) = start >= -50 && endp <= 50
        instructions |> Seq.filter (fun ((xr, yr, zr), _) ->
            insideInitBounds xr && insideInitBounds yr && insideInitBounds zr
        )

    let filteredInput = instructions |> filterInitializationCubes |> Seq.toList

    let grid = new Dictionary<(int * int * int), bool>()
 
    filteredInput 
    |> Seq.iter(fun ((xr,yr,zr), state) -> 
        for x in enumRange xr do
            for y in enumRange yr do
                for z in enumRange zr do
                    grid.[(x,y,z)] <- state
    )

    grid.Values |> Seq.filter id |> Seq.length

let solve1 fileName = 
    let input = getInputPath fileName |> File.ReadAllLines |> Seq.map mapLine
    computeOnCubesWithBruteForce input

assert(solve1 "Day22_Sample1.txt" = 39)
//assert(solve1 "Day22_Sample2.txt" = 590784)
let sw = Stopwatch.StartNew()
//let result = solve1 "Day22.txt"
//printfn "Day 22 Part 1 solution : %i. Took %A" result sw.Elapsed

let intersect (((min_x1,max_x1),(min_y1,max_y1),(min_z1,max_z1)):Cuboid) (((min_x2,max_x2),(min_y2,max_y2),(min_z2,max_z2)):Cuboid)=
    let xsr = max min_x1 min_x2
    let xer = min max_x1 max_x2
    if xsr > xer then
        None
    else
        let ysr = max min_y1 min_y2
        let yer = min max_y1 max_y2
        if ysr > yer then 
            None
        else
            let zsr = max min_z1 min_z2
            let zer = min max_z1 max_z2
            if zsr > zer then
                None
            else
                Some ((xsr, xer),(ysr,yer),(zsr,zer))

// Let's make a few (hopefully valid) tests
assert(intersect ((0,1),(0,1),(0,1)) ((0,2),(0,2),(0,2)) = Some ((0,1),(0,1),(0,1)))
assert(intersect ((0,2),(0,2),(0,2)) ((0,1),(0,1),(0,1)) = Some ((0,1),(0,1),(0,1)))
assert(intersect ((-1,2),(-1,2),(-1,2)) ((0,2),(0,2),(0,2)) = Some ((0,2),(0,2),(0,2)))
assert(intersect ((-1,0),(-1,0),(-1,0)) ((0,2),(0,2),(0,2)) = Some ((0,0),(0,0),(0,0)))
assert(intersect ((-2,2),(-2,2),(-2,2)) ((-1,1),(-1,1),(-1,1)) = Some ((-1,1),(-1,1),(-1,1)))
assert(intersect ((-2,2),(-2,2),(-2,2)) ((5,5),(-1,1),(-1,1)) = None)
assert(intersect ((-2,2),(-2,2),(-2,2)) ((-1,1),(-5,-5),(-1,1)) = None)
assert(intersect ((-2,2),(-2,2),(-2,2)) ((-1,1),(-1,-1),(6,6)) = None)

type SignedCuboid = 
| PositiveCuboid of Cuboid
| NegativeCuboid of Cuboid

let signedIntersect c1 c2 =
    match c1, c2 with
    | PositiveCuboid pc1, PositiveCuboid pc2 -> intersect pc1 pc2 |> Option.map NegativeCuboid
    | NegativeCuboid nc1, NegativeCuboid nc2 -> intersect nc1 nc2 |> Option.map PositiveCuboid
    | PositiveCuboid pc1, NegativeCuboid nc2 -> intersect pc1 nc2 |> Option.map PositiveCuboid
    | NegativeCuboid nc1, PositiveCuboid pc2 -> intersect nc1 pc2 |> Option.map NegativeCuboid

let inline vol3 (((xs,xe), (ys,ye), (zs,ze)):Cuboid) = 
    ((xe-xs+1) |> int64) * ((ye-ys+1)|> int64) * ((ze-zs+1)|> int64)

let computeOnCubesUsingSignedCubesAlgo instructions =
    instructions |> Seq.fold (fun cuboids instr ->
        let ((min_x, max_x), (min_y, max_y), (min_z, max_z)), state = instr

        let currentCuboid:Cuboid = (min_x, max_x), (min_y, max_y), (min_z, max_z)
        let current = 
            match state with 
            | true -> currentCuboid |> PositiveCuboid
            | false -> currentCuboid |> NegativeCuboid

        let intersections = cuboids |> List.choose (fun cuboid -> signedIntersect current cuboid)
 
        List.concat [cuboids;intersections;(if state then [current] else [])]
    ) []
   
let computeTotalVolume cuboids =
    let mutable result = 0L
 
    for cuboid in cuboids do
        match cuboid with
        | PositiveCuboid c ->
            result <- result + vol3 c
        | NegativeCuboid c ->
            result <- result - vol3 c
 
    result

let getInputForFile fileName = 
    getInputPath fileName |> File.ReadAllLines |> Seq.map mapLine |> Seq.toList

let solve2 = getInputForFile >> computeOnCubesUsingSignedCubesAlgo >> computeTotalVolume

assert(solve2 "Day22_sample1.txt" = 39L)
assert(solve2 "Day22_sample4.txt" = 46L)
assert(solve2 "Day22_sample3.txt" = 2758514936282235L)

sw.Restart()
let r2 = solve2 "Day22.txt"
printfn "Day 22 Part 2 solution : %i. Took %A" r2 sw.Elapsed
