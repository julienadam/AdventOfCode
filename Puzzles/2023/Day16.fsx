#time "on"
#load "../../Tools.fs"
#load "../../Tools/Array2DTools.fs"
#load "../../Tools/Directions.fs"

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

let inline (++) (a,b) (c,d) = (a+c, b+d)

let north = (-1,0)
let south = (1,0)
let east = (0,1)
let west = (0,-1)

let move (p:int*int) = function
    | North -> p ++ north
    | South -> p ++ south
    | East -> p ++ east
    | West -> p ++ west


let rec beam (grid:Tile array2d)  =
    let energized = new HashSet<(int*int*Direction)>()

    let rec beamRec (grid:Tile array2d) pos (dir:Direction)  =
        let (nr, nc) = move pos dir
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
    beamRec grid (0,-1) East
    energized

let solve1 input =
    let grid = getInput input 
    let energized = grid |> beam
    energized
    |> Seq.map (fun (r,c,v) -> (r,c)) 
    |> Set.ofSeq 
    |> Set.count

solve1 "Day16_sample1.txt"
solve1 "Day16.txt"

  
// grid |> printGrid
//     printfn ""
//     grid |> printGridCustom2 (fun v r c -> 
//         let found = energized |> Seq.exists (fun (rv, cv, d) -> rv = r && cv = c)
//         match found with
//         | true -> '#'
//         | false -> v.ToString() |> Seq.head
//     ) |> ignore