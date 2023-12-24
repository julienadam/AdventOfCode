#time "on"
#load "../../Tools.fs"
#load "../../Tools/Array2DTools.fs"
#r "nuget: NFluent"

open System.IO
open AdventOfCode
open AdventOfCode.Array2DTools
open NFluent

let getInput name = 
    File.ReadAllLines(getInputPath2023 name)
    |> Array.map (fun line -> line.ToCharArray())
    |> array2D

let getAdjacent (grid:char array2d) r c =
    match grid[r,c] with
    | '>' -> [ r, c + 1 ]
    | '<' -> [ r, c - 1 ]
    | 'v' -> [ r + 1, c ]
    | '^' -> [ r - 1, c ]
    | '.' -> [ r - 1, c; r + 1, c; r, c - 1; r, c + 1 ]
    | _ -> failwith "error"
    
let solve1 input =
    let grid = getInput input 
    // Plug the entrance and exit to avoid bound checks
    grid[0,1] <- '#'
    grid[(grid |> maxR), ((grid |> maxC) - 1)] <- '#'
    let target = (grid |> maxR) - 1, (grid |> maxC) - 1
    
    let rec walk r c path =
        let p = path |> Set.add (r,c)
        if (r,c) = target then
            p
        else
            let adjacent = 
                getAdjacent grid r c
                |> List.filter (fun (ar, ac) -> grid[ar,ac] <> '#' && p.Contains(ar,ac) = false)

            if adjacent.IsEmpty then 
                    Set.empty
                else
                    adjacent 
                    |> Seq.map(fun (ar,ac) -> walk ar ac p)
                    |> Seq.maxBy(fun p -> p.Count)

    let longestPath = walk 1 1 Set.empty
    longestPath.Count + 1

Check.That(solve1 "Day23_sample1.txt").IsEqualTo(94)

solve1 "Day23.txt"

