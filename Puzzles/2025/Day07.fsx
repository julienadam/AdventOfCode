#time "on"
#load "../../Tools.fs"
#load "../../Tools/Array2DTools.fs"
#r "nuget: NFluent"

open System.Collections.Generic
open System.IO
open AdventOfCode
open AdventOfCode.Array2DTools
open Checked
open NFluent

let getInput name =
    File.ReadAllLines(getInputPath2025 name)
    |> Array.map (fun s -> s.ToCharArray())
    |> array2D
    
let solve1 input =
    let grid = getInput input
    let startPos = grid[0,*] |> Array.findIndex(fun c -> c = 'S')
    // startPos |>Dump
    let rec tachyonBeam row positions numberOfSplits =
        if row = (grid |> maxR) then
            numberOfSplits
        else
            let mutable splits = 0;
            let nextPositions =
                positions
                |> Seq.collect (fun p ->
                    match grid[row+1, p] with
                    | '^' ->
                        splits <- splits + 1
                        [p-1;p+1] // split
                    | '.' -> [p] // continue
                    | x -> failwith $"{x} is not a valid cell"
                    )
                |> Set.ofSeq
            tachyonBeam (row+1) nextPositions (numberOfSplits + splits)
        
    tachyonBeam 0 ([startPos] |> Set.ofSeq) 0

Check.That(solve1 "Day07_sample1.txt").IsEqualTo(21)

let solve2 input =
    let grid = getInput input
    let startPos = grid[0,*] |> Array.findIndex(fun c -> c = 'S')
    let memo = Dictionary<int*int, int64>()
    let rec quantumTachyonBeam row position =
        match memo.TryGetValue((row,position)) with
        | true, n -> n
        | _ -> 
            if row = (grid |> maxR) then
                1
            else
                let result =
                    match grid[row + 1, position] with
                    | '^' -> (quantumTachyonBeam (row+1) (position-1)) + (quantumTachyonBeam (row+1) (position+1))
                    | '.' -> (quantumTachyonBeam (row+1) position)
                    | _ -> failwithf "invalid cell"
                memo.TryAdd((row, position), result) |> ignore
                result
        
    quantumTachyonBeam 0 startPos
   
Check.That(solve2 "Day07_sample1.txt").IsEqualTo(40)

solve2 "Day07.txt"