#time "on"
#load "../../Tools.fs"
#load "../../Tools/SeqEx.fs"

open System
open System.IO
open AdventOfCode
open Checked

type Point = int * int
type Range = int * int

type VerticalSegment = Range * int
type HorizontalSegment = int * Range

type Segment = | Vertical of VerticalSegment | Horizontal of HorizontalSegment

let instrToSegments (instructions:string array) =
    let allSegments, _ =
        instructions |> Seq.fold (fun (segments:Segment list, (r,c)) instruction ->
        let steps = instruction.Substring(1) |> int
        match instruction[0] with
        | 'U' -> (((r - steps, r), c) |> Vertical)::segments, (r - steps, c)
        | 'D' -> (((r, r + steps), c) |> Vertical)::segments, (r + steps, c)
        | 'R' -> ((r, (c, c + steps)) |> Horizontal)::segments, (r, c + steps)
        | 'L' -> ((r, (c - steps, c)) |> Horizontal)::segments, (r, c - steps)
        | x -> failwithf $"not a valid direction {x}"
        ) ([], (0,0))
    allSegments

let getInput name =
    File.ReadAllLines(getInputPath2019 name)
    |> Array.map (fun l -> l |> ssplit "," |> instrToSegments)
    |> tupleize2

let tryIntersectVH ((rmin, rmax),c) (r, (cmin,cmax)) =
    if r >= rmin && r <= rmax && c >= cmin && c <= cmax then
        [(r,c)]
    else
        []

let tryIntersectHH (r1, (cmin1, cmax1)) (r2, (cmin2, cmax2)) =
    if r1 <> r2 then
        []
    else
        let m' = max cmin1 cmin2
        let M' = min cmax1 cmax2
        if m' <= M' then
            [m'..M'] |> List.map (fun c -> r1, c)
        else
            []
            
let tryIntersectVV ((rmin1,rmax1), c1) ((rmin2,rmax2), c2) =
    if c1 <> c2 then
        []
    else
        let m' = max rmin1 rmin2
        let M' = min rmax1 rmax2
        if m' <= M' then
            [m'..M'] |> List.map (fun r -> r, c1)
        else
            []
        
// tryIntersectHH (0, (-10, 0)) (0, (-20, -10))
// tryIntersectVH ((0, 10), 5) (5, (6, 10))

let solve1 input =
    let wires1, wires2 = getInput input
    SeqEx.crossproduct wires1 wires2
    |> Seq.collect (fun (s1, s2) ->
        match s1, s2 with
        | Horizontal a, Horizontal b -> tryIntersectHH a b
        | Vertical a, Vertical b -> tryIntersectVV a b
        | Horizontal a, Vertical b -> tryIntersectVH b a
        | Vertical a, Horizontal b -> tryIntersectVH a b
        )
    |> Seq.filter (fun (r,c)-> r <> 0 && c <> 0)
    |> Seq.map (fun (r,c) -> abs r + abs c)
    |> Seq.min

solve1 "Day03_sample1.txt" // 6
solve1 "Day03_sample2.txt" // 159
solve1 "Day03_sample3.txt" // 135
solve1 "Day03.txt"

let stepsTo target segments =
    let stepsOnPath (instruction:string) =
        let steps = instruction.Substring(1) |> int
        match instruction[0] with
        | 'U' -> List.replicate steps (-1,0)
        | 'D' -> List.replicate steps (1,0)
        | 'R' -> List.replicate steps (0,1)
        | 'L' -> List.replicate steps (0,-1)
        | x -> failwithf $"not a valid direction {x}"
    
    let rec walk remainingStepsOnPath remainingInstructions steps (r,c) =
        if (r,c) = target then steps
        else
            match remainingStepsOnPath with
            | [] ->
                match remainingInstructions with
                | [] -> failwithf $"{target} not found on path"
                | head::tail -> walk (stepsOnPath head) tail steps (r,c)
            | (sr,sc)::remaining ->
                walk remaining remainingInstructions (steps+1) (r+sr, c+sc)

    walk [] segments 0 (0,0)
    // let mutable steps = 0
    // let mutable pos = (0,0)
    
#r "nuget: FSharp.Collections.ParallelSeq"

open FSharp.Collections.ParallelSeq

let solve2 input =
    let wires1, wires2 = getInput input
    let intersections =
        SeqEx.crossproduct wires1 wires2
        |> Seq.collect (fun (s1, s2) ->
            match s1, s2 with
            | Horizontal a, Horizontal b -> tryIntersectHH a b
            | Vertical a, Vertical b -> tryIntersectVV a b
            | Horizontal a, Vertical b -> tryIntersectVH b a
            | Vertical a, Horizontal b -> tryIntersectVH a b
            )
        |> Seq.filter (fun (r,c)-> r <> 0 && c <> 0)
    
    let instructions1, instructions2 =
        File.ReadAllLines(getInputPath2019 input)
        |> Array.map (fun l -> l |> ssplit "," |> Array.toList)
        |> tupleize2
        
    intersections
    |> PSeq.map (fun (r,c) -> stepsTo (r,c) instructions1 + stepsTo (r,c) instructions2)
    |> Seq.min

solve2 "Day03_sample1.txt" // 30
solve2 "Day03_sample2.txt" // 610
solve2 "Day03_sample3.txt" // 410

solve2 "Day03.txt"