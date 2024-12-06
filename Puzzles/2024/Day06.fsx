#time "on"
#load "../../Tools.fs"
#load "../../Tools/Array2DTools.fs"
#load "../../Tools/Directions.fs"
#r "nuget: FSharp.Collections.ParallelSeq"

open System.IO
open AdventOfCode
open Array2DTools
open FSharp.Collections.ParallelSeq

let getInput name = 
    File.ReadAllLines(getInputPath2024 name)
    |> array2D

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
        |> Dump

    // set the current guard cell to empty, assuming the guard isn't
    // standing on a pile of discarded prototype suits
    Array2D.set grid startR startC '.'

    let visited = patrol grid (startR, startC) Direction.North (Set.empty |> Set.add (startR, startC))
    visited.Count

solve1 "Day06.txt"

let rec isPatrolLoop grid (r,c) (dir:Direction) (visited:(int*int*Direction) Set) =
    if visited |> Set.contains (r,c,dir) then
        true
    else 
        let nv = visited |> Set.add (r,c,dir)
        let (nr, nc) = dir.Move (r,c)
        if grid |> isInBounds nr nc |> not then
            false
        else
            match grid[nr,nc] with
            | '.' -> 
                isPatrolLoop grid (nr,nc) dir nv
            | '#' -> 
                let nd = dir.TurnRight()
                isPatrolLoop grid (r,c) nd nv
            | x -> failwithf "Not a valid cell %c" x

let solve2 input =
    let grid = getInput input
    let (startR, startC, _) = 
        grid 
        |> Array2DTools.enumArray2d 
        |> Seq.find (fun (_,_,v) -> v = '^')
        |> Dump

    // set the current guard cell to empty, assuming the guard isn't
    // standing on a pile of discarded prototype suits
    Array2D.set grid startR startC '.'

    let effectiveObstacles = 
        [0..maxR grid] |> PSeq.collect (fun r ->
            printfn "%i" r
            let gridCopy = Array2D.copy grid
            [0..maxC grid] |> Seq.choose (fun c ->
                if gridCopy[r,c] = '.' then
                    gridCopy[r,c] <- '#'
                    if isPatrolLoop gridCopy (startR, startC) Direction.North Set.empty then
                        gridCopy[r,c] <- '.'
                        Some (r,c)
                    else
                        gridCopy[r,c] <- '.'
                        None
                else
                    None
        ))

    effectiveObstacles // |> Seq.toArray |> Dump
    |> PSeq.length

solve2 "Day06.txt"