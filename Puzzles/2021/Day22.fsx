

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

let intersect (((xs1,xe1),(ys1,ye1),(zs1,ze1)):Cuboid) (((xs2,xe2),(ys2,ye2),(zs2,ze2)):Cuboid) : Cuboid option =
    let xsr = max xs1 xs2
    let xer = min xe1 xe2
    if xsr > xer then
        None
    else
        let ysr = max ys1 ys2
        let yer = min ye1 ye2
        if ysr > yer then 
            None
        else
            let zsr = max zs1 zs2
            let zer = min ze1 ze2
            if zsr > zer then
                None
            else
                Some ((xsr, xer),(ysr,yer),(zsr,zer))

assert(intersect ((0,1),(0,1),(0,1)) ((0,2),(0,2),(0,2)) = Some ((0,1),(0,1),(0,1)))
assert(intersect ((0,2),(0,2),(0,2)) ((0,1),(0,1),(0,1)) = Some ((0,1),(0,1),(0,1)))
assert(intersect ((-1,2),(-1,2),(-1,2)) ((0,2),(0,2),(0,2)) = Some ((0,2),(0,2),(0,2)))
assert(intersect ((-1,0),(-1,0),(-1,0)) ((0,2),(0,2),(0,2)) = Some ((0,0),(0,0),(0,0)))
assert(intersect ((-2,2),(-2,2),(-2,2)) ((-1,1),(-1,1),(-1,1)) = Some ((-1,1),(-1,1),(-1,1)))
assert(intersect ((-2,2),(-2,2),(-2,2)) ((5,5),(-1,1),(-1,1)) = None)
assert(intersect ((-2,2),(-2,2),(-2,2)) ((-1,1),(-5,-5),(-1,1)) = None)
assert(intersect ((-2,2),(-2,2),(-2,2)) ((-1,1),(-1,-1),(6,6)) = None)

let chop (instructions: RebootInstruction list) =

    let xPoints = 
        instructions 
        |> Seq.collect (fun (((xs,xe), _, _), _) -> [xs;xe]) 
        |> Seq.distinct 
        |> Seq.sort 
        |> Seq.mapi (fun i v -> if i = 0 then v - 1 else v)
    let yPoints = 
        instructions 
        |> Seq.collect (fun ((_, (ys,ye), _), _) -> [ys;ye]) 
        |> Seq.distinct 
        |> Seq.sort 
        |> Seq.mapi (fun i v -> if i = 0 then v - 1 else v)
    let zPoints = 
        instructions 
        |> Seq.collect (fun ((_, _, (zs,ze)), _) -> [zs;ze]) 
        |> Seq.distinct 
        |> Seq.sort 
        |> Seq.mapi (fun i v -> if i = 0 then v - 1 else v)

    //printfn "%i pairs on X" (xPoints |> Seq.pairwise |> Seq.length)
    //printfn "%i pairs on Y" (yPoints |> Seq.pairwise |> Seq.length)
    //printfn "%i pairs on Z" (zPoints |> Seq.pairwise |> Seq.length)

    seq {
        for (xs,xe) in xPoints |> Seq.pairwise do
            for (ys,ye) in yPoints |> Seq.pairwise do
                for (zs,ze) in zPoints |> Seq.pairwise do
                    yield ((xs + 1,xe), (ys + 1,ye), (zs + 1,ze)) |> Cuboid
    }

let inline vol3 ((xs,xe),(ys,ye),(zs,ze)) = (xe-xs)*(ye-ys)*(ze-zs)

let solve2 fileName = 
    // Idea 0 :
    // brute force each pixel, traversing each cube. Not likely
    // 250000*250000*250000 means millions of billions of cubes

    // Idea 1 : 
    //  take the cubes by pairs
    //  keep non-intersecting cubes
    //  if intersecting, cut them up in smaller, non-intersecting cubes 
    //  take all of these and sum the volumes of the ON ones ?

    // Idea 2 :
    // Count cubes occupied by first instr
    // Take next cube, count cubes occupied by this cube
        // Intersect with each previous cube


        //// If there is an intersection compute the result
        //  // if this in ON and prev is ON : add (this.Volume - intersect.Vol)
        //  // if this in OFF and prev is OFF : nothing to do
        //  // if this in ON and prev is OFF : nothing to do
        // ... sounds complicated
          
    // Idea 3 :
    // Divide space along x using all min and max x values of all cubes
    // Divide space along y using all min and max y values of all cubes
    // Divide space along z using all min and max z values of all cubes
    let input = getInputPath fileName |> File.ReadAllLines |> Seq.map mapLine |> Seq.toList

    let distinctCubes = input |> chop |> Dump
    
    // Use these to divide space in distinct, non-intersecting cubes
    // Intersect all the non-intersecting cubes with each cube in the input, starting from the end
    // the first one to intersect gives us the state of the cube
    let reversedInput = input |> List.rev
    let volumes = 
        distinctCubes |> Seq.map (fun c ->
            match reversedInput |> Seq.tryPick (fun (cuboid, state) -> intersect cuboid c |> Option.map (fun _ -> state)) with
            | Some onOff ->
                if onOff then
                    (vol3 c) |> int64
                else
                    0L
            | _ -> 
                printfn "No intersection found for %A" c
                0L
        )
    volumes |> Seq.sum

    //let (xs,xe) = input |> Seq.map (fun (((xs,_), _, _), _) -> xs) |> Seq.min, input |> Seq.map (fun (((_,xe), _, _), _) -> xe) |> Seq.max
    //let (ys,ye) = input |> Seq.map (fun ((_, (ys,_), _), _) -> ys) |> Seq.min, input |> Seq.map (fun ((_, (_,ye), _), _) -> ye) |> Seq.max
    //let (zs,ze) = input |> Seq.map (fun ((_, _, (zs,_)), _) -> zs) |> Seq.min, input |> Seq.map (fun ((_, _, (_,ze)), _) -> ze) |> Seq.max
    //printfn "X: %A Y: %A Z: %A" (xs,xe) (ys,ye) (zs,ze) 
    

sw.Restart()
let r2 = solve2 "Day22_sample1.txt"
printfn "Day 22 Part 2 sample  : %i. Took %A" r2 sw.Elapsed
// TODO : filter again for init cubes after reboot proc. Shouldn't be necessary as the coords don't change