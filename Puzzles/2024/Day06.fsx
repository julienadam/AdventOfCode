#time "on"
#load "../../Tools.fs"
#load "../../Tools/Array2DTools.fs"
#load "../../Tools/Directions.fs"
#r "nuget: FSharp.Collections.ParallelSeq"

open System.IO
open AdventOfCode
open Array2DTools
open FSharp.Collections.ParallelSeq

let getInput name = File.ReadAllLines(getInputPath2024 name) |> array2D

let rec patrol grid (r,c) (dir:Direction) (visited:(int*int) Set) =
    let (nr, nc) = dir.Move (r,c)
    if grid |> isInBounds nr nc |> not then
        visited
    else
        match grid[nr,nc] with
        | '.' -> patrol grid (nr,nc) dir (visited |> Set.add (nr,nc))
        | '#' -> patrol grid (r,c) (dir.TurnRight()) visited
        | x -> failwithf "Not a valid cell %c" x

let solve1 input =
    let grid = getInput input
    let (startR, startC, _) = 
        grid 
        |> Array2DTools.enumArray2d 
        |> Seq.find (fun (_,_,v) -> v = '^')

    // set the current guard cell to empty, assuming the guard isn't
    // standing on a pile of discarded prototype suits
    Array2D.set grid startR startC '.'

    let visited = patrol grid (startR, startC) Direction.North (Set.empty |> Set.add (startR, startC))
    visited.Count

solve1 "Day06.txt"

let dirToInt = function | North -> 1 | East -> 2 | South -> 4 | West -> 8

let isPatrolLoopInt (grid:int[,]) (sr,sc) =
    let allDirs = 1 ||| 2 ||| 4 ||| 8
    let rec isPatrolLoopIntRec (r,c) dir =
        let dirInt = dirToInt dir
        let curr = grid[r,c]
        // Check if the current cell was already passed in that direction
        // using bitwise ops
        if curr &&& dirInt = dirInt then
            true
        else 
            // Every time we pass a cell, record the passage by 
            // setting the direction bit in the cell itseld
            grid[r,c] <- curr ||| dirInt
            let (nr, nc) = dir.Move (r,c)
            if grid |> isInBounds nr nc |> not then
                false
            else
                match grid[nr,nc] with
                | x when x <= allDirs -> isPatrolLoopIntRec (nr,nc) dir
                | System.Int32.MaxValue -> isPatrolLoopIntRec (r,c) (dir.TurnRight())
                | x -> failwithf "Not a valid cell %i" x
    isPatrolLoopIntRec (sr,sc) Direction.North

let solve2 input =
    let grid = getInput input
    let (startR, startC, _) = 
        grid 
        |> Array2DTools.enumArray2d 
        |> Seq.find (fun (_,_,v) -> v = '^')

    // set the current guard cell to empty, assuming the guard isn't
    // standing on a pile of discarded prototype suits
    Array2D.set grid startR startC '.'

    // Let's optimize this a little, we don't need to try all possible obstacle positions
    // Only the cells that are on the patrol route are potentiel targets
    let targets = 
        patrol grid (startR, startC) Direction.North (Set.empty |> Set.add (startR, startC))
        |> Set.remove (startR, startC)

    printfn "%i possible targets, compared to %i initially" targets.Count ((maxR grid)*(maxC grid))

    // Transform the grid into an int grid, we're going to use the grid itself to store the route data
    let grid = 
        grid |> Array2D.map (
            function
            | '.' -> 0
            | '#' -> System.Int32.MaxValue
            | x -> failwithf "Not a valid cell %c" x
        )

    let effectiveObstacles = 
        targets |>PSeq.choose(fun (r,c) -> 
            let gridCopy = Array2D.copy grid
            gridCopy[r,c] <- System.Int32.MaxValue
            if isPatrolLoopInt gridCopy (startR, startC) then
                Some (r,c)
            else
                None
        )

    effectiveObstacles // |> Seq.toArray |> Dump
    |> PSeq.length

solve2 "Day06.txt"