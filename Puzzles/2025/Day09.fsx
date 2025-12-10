#time "on"
#load "../../Tools.fs"
#load "../../Tools/Array2DTools.fs"
#load "../../Tools/SeqEx.fs"
#r "nuget: NFluent"
#r "nuget: FSharp.Collections.ParallelSeq"

open System
open System.IO
open AdventOfCode
open Checked
open FSharp.Collections.ParallelSeq
open NFluent

let getInput name =
    File.ReadLines(getInputPath2025 name)
    |> Seq.map (splitIntList64 >> tupleize2 >> swap2)
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
    
type Point = int64*int64
type Segment = Point*Point 

// Compress the coordinates in the sequence of points
let compress (points:Point seq) =
    let inline indexEven ints = ints |> Seq.distinct |> Seq.sort |> Seq.indexed |> Seq.map (fun (a,b) -> int64 (a * 2), b)
    let compressedRows = points |> Seq.map fst |> indexEven
    let compressedColumns = points |> Seq.map snd |> indexEven
    let colMap = compressedColumns |> Seq.map swap2 |> Map.ofSeq
    let rowMap = compressedRows |> Seq.map swap2 |> Map.ofSeq
    points |> Seq.map (fun (r,c) -> (r,c),(rowMap[r], colMap[c]))

/// transform the list of points into a list of segments forming a polygon
/// including a segment from the last point to the first to close the polygon
let pointListToSegments points =
     points
     |> Seq.pairwise
     |> Seq.append [(points |> Seq.last, points |> Seq.head)]

/// fill a grid with the polygon
let draw segments (grid:bool array2d) =
     for (r1,c1),(r2,c2) in segments do
        if r1 = r2 then
            for c = int (min c1 c2) to int (max c1 c2) do
                grid[int r1,c] <- true
        else if c1 = c2 then
            for r = int (min r1 r2) to int (max r1 r2) do
                grid[r, int c1] <- true
        else failwithf "Segment was neither H not V"
        
/// Fill the grid at the starting point, in all directions
let rec floodFill r c (grid:bool array2d) =
    grid[r,c] <- true
    match grid |> Array2DTools.tryGetRight r c with
    | Some (rr,cr, false) -> floodFill rr cr grid
    | _ -> ()
    
    match grid |> Array2DTools.tryGetUp r c with
    | Some (ru,cu, false) -> floodFill ru cu grid
    | _ -> ()
    
    match grid |> Array2DTools.tryGetDown r c with
    | Some (rd,cd, false) -> floodFill rd cd grid
    | _ -> ()
    
    match grid |> Array2DTools.tryGetLeft r c with
    | Some (rl,cl, false) -> floodFill rl cl grid
    | _ -> ()
    
/// Enumerate tiles in the designated rectangle that are NOT inside the polygon drawn on the grid
let findBadTilesInRectangle ((sr,sc),(er,ec)) (grid:bool array2d) =
    seq {
        for r = min sr er to max sr er do
            for c = min sc ec to max sc ec do
                if grid[r,c] = false then
                    yield (r,c)
    }

let i64tuple (a,b) = (int64 a), (int64 b)

let solve2 input =
    let points = getInput input |> Seq.map swap2
    // Compress the coordinates
    let compressed = compress points
    let segments = compressed |> Seq.map snd |> pointListToSegments
    let maxR = compressed |> Seq.map (fun a -> fst (snd a)) |> Seq.max |> int
    let maxC = compressed |> Seq.map (fun a -> snd (snd a)) |> Seq.max |> int

    let grid = Array2D.create (maxR+1) (maxC+1) false
    draw segments grid
    
    // Find the middle of a vertical segment draw on the left edge
    let (sr,_),(er,_) =
        segments
        |> Seq.find (fun ((r1,c1),(r2,c2)) -> c1 = c2 && c1 = 0)
    
    // Use it as the starting point for the flood fill
    let startR = (min sr er) + ((abs (sr - er)) / 2L)
    // Fill the compressed grid 
    floodFill (int startR) 0 grid
    
    let compressionMap = compressed |> Seq.map swap2 |> Map.ofSeq
    let decompressArea (p1:int*int,p2:int*int) = area (compressionMap[p1 |> i64tuple] ) (compressionMap[p2|> i64tuple])
    
    let compressedPoints = compressed |> Seq.map snd |> Seq.map (fun (r,c) -> int r, int c)|> Seq.toArray
    
    // Try all rectangles 
    SeqEx.autoProduct compressedPoints
    |> Seq.filter (fun (a,b) -> a <> b)// eliminate p,p tuples
    |> PSeq.filter (fun rect ->(findBadTilesInRectangle rect grid |> Seq.tryHead).IsNone) // eliminate any that have bad tiles
    |> Seq.map decompressArea // compute real area from the compressed coordinates
    |> Seq.max

Check.That(solve2 "Day09_sample1.txt").IsEqualTo(24)
solve2 "Day09.txt"
