#time
#load "../../Tools.fs"
#load "../../Tools/Array2DTools.fs"

open System.IO
open AdventOfCode
open Array2DTools
open System.Collections.Generic

let empty = '.'
let rolling = 'O'

let getInput name = 
    File.ReadAllLines(getInputPath2023 name)
    |> Array.map Seq.toArray
    |> array2D

let tiltColumnTowardsLim (grid:char array2d) pos isSwap startPos endPos step =
    let rec tiltColumnTowardsStartRec freePos currPos =
        if currPos = endPos then 
            ()
        else
            let v = if isSwap then grid[pos, currPos] else grid[currPos, pos]
            match (v, freePos) with
            | 'O', Some f -> 
                if isSwap then
                    grid[pos, f] <- rolling
                    grid[pos,currPos] <- empty
                else
                    grid[f, pos] <- rolling
                    grid[currPos, pos] <- empty
                tiltColumnTowardsStartRec (Some(f + step)) (currPos + step)
            | 'O', None
            | '#', _ -> tiltColumnTowardsStartRec None (currPos + step)
            | '.', Some f -> tiltColumnTowardsStartRec (Some f) (currPos + step)
            | '.', None -> tiltColumnTowardsStartRec (Some currPos) (currPos + step)
            | _ -> failwithf "nope"
    tiltColumnTowardsStartRec None startPos

let tiltNorth (grid:char array2d) =
    for i = 0 to (grid |> maxC) do
        tiltColumnTowardsLim grid i false 0 (grid |> lenC) 1
    grid

let tiltWest (grid:char array2d) =
    for i = 0 to (grid |> maxR) do
        tiltColumnTowardsLim grid i true 0 (grid |> lenR) 1
    grid

let tiltSouth (grid:char array2d) =
    for i = 0 to (grid |> maxC) do
        tiltColumnTowardsLim grid i false (grid |> maxC) -1 -1
    grid

let tiltEast (grid:char array2d) =
    for i = 0 to (grid |> maxR) do
        tiltColumnTowardsLim grid i true (grid |> maxC) -1 -1
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
    let states = new Dictionary<int, (int * char array2d)>()
    let mutable cycleFound = false
    let mutable i = 0
    let mutable result = (0,0)
    while (cycleFound = false) do 
        i <- i + 1
        grid <- cycle grid 
        let hash = hashGrid grid
        match states.TryGetValue hash with
        | true, (round, _) ->
            cycleFound <- true
            result <- (round, i)
        | false, _ ->
            states.Add(hash, (i, grid.Clone() :?> char array2d))
    result, states

let solve2 input =
    let mutable grid = getInput input
    let (cycleStart, cycleEnd), states = findCycleLength (grid.Clone() :?> char array2d)
    let length = (cycleEnd - cycleStart)
    let remaining = (cycleStart - 1) + (1000000000 - cycleStart + 1) % length
    states.Values |> Seq.find (fun (r,x) -> r = remaining) |> snd |> calcLoad

if (solve2 "Day14_sample1.txt") <> 64 then failwithf "Invalid sample result for part 2"

solve1 "Day14.txt"
solve2 "Day14.txt"