#time "on"
#load "../../Tools.fs"
#load "../../Tools/Array2DTools.fs"
#load "../../Tools/Directions.fs"
#r "nuget: FSharp.Collections.ParallelSeq"

open System
open System.IO
open AdventOfCode
open System.Collections.Generic
open Array2DTools

type Tile =
    | Empty
    | VerticalSplitter
    | HorizontalSplitter
    | Slash
    | Backslash
    with
        override this.ToString() =
            match this with
            | Empty -> "."  
            | HorizontalSplitter ->  "-"  
            | VerticalSplitter ->  "|"  
            | Slash ->  "/"  
            | Backslash ->  "\\"

let parseTile = function 
    | '.'  -> Empty
    | '-'  -> HorizontalSplitter
    | '|'  -> VerticalSplitter
    | '/'  -> Slash
    | '\\' -> Backslash
    | _    -> failwith "Illegal character"

let getInput name = 
    File.ReadAllLines(getInputPath2023 name)
    |> Array.map (Seq.map parseTile >> Seq.toArray)
    |> array2D
let rec beam pos dir (grid:Tile array2d)  =
    let energized = new HashSet<(int*int*Direction)>()

    let rec beamRec (grid:Tile array2d) pos (dir:Direction)  =
        let (nr, nc) = pos |> dir.Move
        if nr < 0 || nc < 0 || nr > (grid |> maxR) || nc > (grid |> maxC) then
            ()
        else
            if energized.Add(nr, nc, dir) = false then
                ()
            else
                match grid[nr,nc], dir with
                | Empty, _
                | HorizontalSplitter, East 
                | HorizontalSplitter, West 
                | VerticalSplitter, North
                | VerticalSplitter, South -> beamRec grid (nr,nc) dir
                | Slash, North -> beamRec grid (nr,nc) East
                | Slash, East -> beamRec grid (nr,nc) North
                | Slash, West -> beamRec grid (nr,nc) South
                | Slash, South -> beamRec grid (nr,nc) West
                | Backslash, North -> beamRec grid (nr,nc) West
                | Backslash, East -> beamRec grid (nr,nc) South
                | Backslash, West -> beamRec grid (nr,nc) North
                | Backslash, South -> beamRec grid (nr,nc) East
                | VerticalSplitter, _ -> 
                    beamRec grid (nr,nc) North
                    beamRec grid (nr,nc) South
                | HorizontalSplitter, _ -> 
                    beamRec grid (nr,nc) East
                    beamRec grid (nr,nc) West
    beamRec grid pos dir
    energized
    |> Seq.map (fun (r,c,v) -> (r,c)) 
    |> Set.ofSeq 
    |> Set.count

let solve1 input =
    let grid = getInput input 
    grid |> beam (0,-1) East
   

solve1 "Day16_sample1.txt"
solve1 "Day16.txt"

open FSharp.Collections.ParallelSeq

let solve2 input =
    let grid = getInput input 
    let maxCoord = grid |> maxC
    [0..maxCoord] 
    |> PSeq.collect (fun i -> 
        [
            grid |> beam (i, -1) East
            grid |> beam (i, maxCoord+1) West
            grid |> beam (-1, i) South
            grid |> beam (maxCoord+1, i) North
        ]
    )
    |> Seq.max

solve2 "Day16_sample1.txt"
solve2 "Day16.txt"