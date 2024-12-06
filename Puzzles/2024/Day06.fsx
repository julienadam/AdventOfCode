#time "on"
#load "../../Tools.fs"
#load "../../Tools/Array2DTools.fs"
#load "../../Tools/Directions.fs"

open System.IO
open AdventOfCode
open Array2DTools

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
    let effectiveObstacles = seq {
        for r in [0..maxR grid] do
            printfn "%i" r
            for c in [0..maxC grid] do
                if grid[r,c] = '.' then
                    grid[r,c] <- '#'
                    if isPatrolLoop grid (startR, startC) Direction.North Set.empty then
                        yield (r,c)
                    grid[r,c] <- '.'
    }

    effectiveObstacles // |> Seq.toArray |> Dump
    |> Seq.length


solve2 "Day06.txt"