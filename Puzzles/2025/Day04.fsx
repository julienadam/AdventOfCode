#time "on"
#load "../../Tools.fs"
#load "../../Tools/Array2DTools.fs"
#r "nuget: NFluent"

open System
open System.IO
open AdventOfCode
open Checked
open NFluent
open Array2DTools

let getInput name =
    File.ReadAllLines(getInputPath2025 name)
    |> Array.map (fun s -> s |> Seq.toArray)
    |> array2D

let hasLessThanFourAdjacentRolls grid r c a =
    a = '@' && (grid |> getAdjacentValuesWithDiagonal r c |> Seq.filter (fun v -> v = '@') |> Seq.length < 4)

let solve1 input =
    let grid = getInput input
    grid |> countWhere (hasLessThanFourAdjacentRolls grid)

Check.That(solve1 "Day04_sample1.txt").IsEqualTo(13)

solve1 "Day04.txt"

let solve2 input =
    let grid = getInput input
    
    let rec removeRolls removed =
        let toRemove =
           grid
           |> filteri (hasLessThanFourAdjacentRolls grid)
           |> Seq.toArray
        
        if toRemove.Length = 0 then
            removed
        else
            toRemove |> Seq.iter (fun (r,c,_) -> grid[r,c] <- '.')
            removeRolls (removed + toRemove.Length)
    
    removeRolls 0        

Check.That(solve2 "Day04_sample1.txt").IsEqualTo(43)
solve2 "Day04.txt"
