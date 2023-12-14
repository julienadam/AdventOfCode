
#time
#load "../../Tools.fs"
#load "../../Tools/Array2DTools.fs"

open System
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

let calcLoad (grid:char array2d) =
    grid 
    |> enumArray2d
    |> Seq.sumBy(fun (r, c, v) ->
        if v = rolling then
            (grid |> lenC) - r
        else
            0
    )

let compact (line: char array) =
    let rec compactRec freePos currPos =
        if currPos = (line.Length) then 
            ()
        else
            match (line[currPos], freePos) with
            | 'O', Some f -> 
                line[f] <- rolling
                line[currPos] <- empty
                compactRec (Some(f + 1)) (currPos + 1)
            | 'O', None
            | '#', _ -> compactRec None (currPos + 1)
            | '.', Some f -> compactRec (Some f) (currPos + 1)
            | '.', None -> compactRec (Some currPos) (currPos + 1)
            | _ -> failwithf "nope"
    compactRec None 0
    line


let tiltNorth (grid: char array2d) =
    [|0..grid |> maxC|] 
    |> Array.map (fun c -> grid[*, c] |> compact)
    |> array2D 
    |> transpose

let solve1 input =
    getInput input 
    |> tiltNorth
    |> calcLoad

if solve1 "Day14_sample1.txt" <> 136 then failwithf "Incorrect solution"

solve1 "Day14.txt"

let tiltWest (grid: char array2d) =
    [|0..grid |> maxR|] 
    |> Array.map (fun r -> compact grid[r, *])
    |> array2D

let tiltEast (grid: char array2d) =
    [|0..grid |> maxR|] 
    |> Array.map (fun r -> grid[r, *] |> Array.rev |> compact |> Array.rev)
    |> array2D

let tiltSouth (grid: char array2d) =
    [|0..grid |> maxC|] 
    |> Array.map (fun c -> grid[*, c] |> Array.rev |> compact |> Array.rev)
    |> array2D 
    |> transpose

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

solve2 "Day14_sample1.txt"

solve2 "Day14.txt"
