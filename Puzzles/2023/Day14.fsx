#time
#load "../../Tools.fs"
#load "../../Tools/Array2DTools.fs"

open System.IO
open AdventOfCode
open Array2DTools
open System.Collections.Generic
open System.Text

let empty = '.'
let rolling = 'O'

let getInput name = 
    File.ReadAllLines(getInputPath2023 name)
    |> Array.map Seq.toArray
    |> array2D

let tiltColumnNorth (grid:char array2d) col  =
    let rec tiltColumnNorthRec freePos currPos =
        if currPos = (grid |> lenR) then 
            ()
        else
            match (grid[currPos, col], freePos) with
            | 'O', Some f -> 
                grid[f, col] <- rolling
                grid[currPos, col] <- empty
                tiltColumnNorthRec (Some(f + 1)) (currPos + 1)
            | 'O', None
            | '#', _ -> tiltColumnNorthRec None (currPos + 1)
            | '.', Some f -> tiltColumnNorthRec (Some f) (currPos + 1)
            | '.', None -> tiltColumnNorthRec (Some currPos) (currPos + 1)
            | _ -> failwithf "nope"
    tiltColumnNorthRec None 0

let tiltNorth (grid:char array2d) =
    for i = 0 to (grid |> maxC) do
        tiltColumnNorth grid i
    grid

let tiltColumnSouth (grid:char array2d) col  =
    let rec tiltColumnSouthRec freePos currPos =
        if currPos = -1 then 
            ()
        else
            match (grid[currPos, col], freePos) with
            | 'O', Some f -> 
                grid[f, col] <- rolling
                grid[currPos, col] <- empty
                tiltColumnSouthRec (Some(f - 1)) (currPos - 1)
            | 'O', None
            | '#', _ -> tiltColumnSouthRec None (currPos - 1)
            | '.', Some f -> tiltColumnSouthRec (Some f) (currPos - 1)
            | '.', None -> tiltColumnSouthRec (Some currPos) (currPos - 1)
            | _ -> failwithf "nope"
    tiltColumnSouthRec None (grid |> maxC)

let tiltSouth (grid:char array2d) =
    for i = 0 to (grid |> maxC) do
        tiltColumnSouth grid i
    grid

let tiltRowWest (grid:char array2d) row  =
    let rec tiltRowWestRec freePos currPos =
        if currPos = (grid |> lenC) then 
            ()
        else
            match (grid[row, currPos], freePos) with
            | 'O', Some f -> 
                grid[row, f] <- rolling
                grid[row, currPos] <- empty
                tiltRowWestRec (Some(f + 1)) (currPos + 1)
            | 'O', None
            | '#', _ -> tiltRowWestRec None (currPos + 1)
            | '.', Some f -> tiltRowWestRec (Some f) (currPos + 1)
            | '.', None -> tiltRowWestRec (Some currPos) (currPos + 1)
            | _ -> failwithf "nope"
    tiltRowWestRec None 0

let tiltWest (grid:char array2d) =
    for i = 0 to (grid |> maxR) do
        tiltRowWest grid i
    grid

let tiltRowEast (grid:char array2d) row  =
    let rec tiltRowEastRec freePos currPos =
        if currPos = -1 then 
            ()
        else
            match (grid[row, currPos], freePos) with
            | 'O', Some f -> 
                grid[row, f] <- rolling
                grid[row, currPos] <- empty
                tiltRowEastRec (Some(f - 1)) (currPos - 1)
            | 'O', None
            | '#', _ -> tiltRowEastRec None (currPos - 1)
            | '.', Some f -> tiltRowEastRec (Some f) (currPos - 1)
            | '.', None -> tiltRowEastRec (Some currPos) (currPos - 1)
            | _ -> failwithf "nope"
    tiltRowEastRec None (grid |> maxC)

let tiltEast (grid:char array2d) =
    for i = 0 to (grid |> maxR) do
        tiltRowEast grid i
    grid

let calcLoad (grid:char array2d) =
    grid 
    |> enumArray2d
    |> Seq.sumBy(fun (r, c, v) ->
        if v = rolling then
            (grid |> lenC) - r
        else
            0
    )

let solve1 input =
    getInput input 
    |> tiltNorth
    |> Dump
    |> calcLoad

if solve1 "Day14_sample1.txt" <> 136 then failwithf "Incorrect solution"

let cycle = tiltNorth >> tiltWest >> tiltSouth >> tiltEast

let hashGrid grid =
    let p = 16777619
    let mutable hash = 2166136261L |> int

    grid |> Array2D.iteri (fun r c v ->
        hash <- (hash ^^^ (grid[r,c]|> int)) * p
    )
    hash

let findCycleLength grid =
    let mutable grid = grid
    let states = new Dictionary<int, int>()
    let mutable cycleFound = false
    let mutable i = 0
    let mutable result = (0,0)
    while (cycleFound = false) do 
        i <- i + 1
        grid <- cycle grid 
        let hash = hashGrid grid
        match states.TryGetValue hash with
        | true, round ->
            cycleFound <- true
            result <- (round, i)
        | false, _ ->
            states.Add(hash, i)
    result

let solve2 input =
    let mutable grid = getInput input
    let cycleStart, cycleEnd = findCycleLength (grid.Clone() :?> char array2d)
    let length = (cycleEnd - cycleStart)
    let remaining = (cycleStart - 1) + (1000000000 - cycleStart + 1) % length
    for _ = 1 to remaining do 
        grid <- cycle grid
    calcLoad grid

if (solve2 "Day14_sample1.txt") <> 64 then failwithf "Invalid sample result for part 2"

solve1 "Day14.txt"
solve2 "Day14.txt"