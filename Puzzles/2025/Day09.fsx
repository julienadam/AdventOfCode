#time "on"
#load "../../Tools.fs"
#load "../../Tools/SeqEx.fs"
#r "nuget: NFluent"

open System
open System.IO
open AdventOfCode
open Checked
open NFluent

let getInput name =
    File.ReadLines(getInputPath2025 name)
    |> Seq.map (splitIntList64 >> tupleize2)
    |> Seq.toArray

let area (x1,y1) (x2,y2) = (abs (x2-x1) + 1L) * (abs (y2-y1) + 1L)

let solve1 input =
    let redTiles = getInput input
    SeqEx.autoProduct redTiles
    |> Seq.filter (fun (a,b) -> a <> b)
    |> Seq.map (fun (a,b) -> area a b)
    |> Seq.max
    
Check.That(solve1 "Day09_sample1.txt").IsEqualTo(50)

solve1 "Day09.txt"

let pointOnSegment (r,c) ((r1,c1),(r2,c2)) =
    if r1 = r2 then
        r = r1 && c >= min c1 c2 && c <= max c1 c2
    else if c1 = c2 then
        c = c1 && r >= min r1 r2 && r <= max r1 r2
    else
        failwith $"Segment {r1},{c1} {r2},{c2} is not horizontal or vertical"

Check.That(pointOnSegment (5,4) ((1,4),(9,4))).IsTrue
Check.That(pointOnSegment (1,4) ((1,4),(9,4))).IsTrue
Check.That(pointOnSegment (9,4) ((1,4),(9,4))).IsTrue
Check.That(pointOnSegment (0,4) ((1,4),(9,4))).IsFalse
Check.That(pointOnSegment (10,4) ((1,4),(9,4))).IsFalse
Check.That(pointOnSegment (4,5) ((4,1),(4,9))).IsTrue
Check.That(pointOnSegment (4,1) ((4,1),(4,9))).IsTrue
Check.That(pointOnSegment (4,9) ((4,1),(4,9))).IsTrue
Check.That(pointOnSegment (4,0) ((4,1),(4,9))).IsFalse
Check.That(pointOnSegment (4,10) ((4,1),(4,9))).IsFalse

let pointListToSegments points =
    points
    |> Seq.pairwise
    |> Seq.append [(points |> Seq.last, points |> Seq.head)]

let pointInPolygon (r,c) polygon =
    false

let solve2 input =
    let redTiles = getInput input
    pointListToSegments redTiles |> Seq.toArray
    // |> Seq.filter (fun (a,b) -> a <> b)
    // |> Seq.map (fun (a,b) -> area a b)
    // |> Seq.max
    
Check.That(solve2 "Day09_sample1.txt").IsEqualTo(24)

solve2 "Day09.txt"
