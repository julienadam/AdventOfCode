#time "on"
#load "../../Tools.fs"
#load "../../Tools/Array2DTools.fs"

open System
open System.IO
open AdventOfCode
open Array2DTools

let getInput name = 
    File.ReadAllLines(getInputPath2024 name)
    |> array2D

let dirs = [ (0,1); (0,-1); (1,0); (-1,0); (1,1); (1,-1); (-1,1); (-1,-1) ]

let solve1 input =
    let grid = getInput input 
    let xCoords = 
        grid
        |> Array2DTools.enumArray2d
        |> Seq.filter (fun (_,_,v) -> v = 'X')

    Seq.allPairs xCoords dirs
    |> Seq.filter (fun ((r,c,_), (dr,dc)) ->
        let (nr,nc) = (r+3*dr, c+3*dc)
        grid |> isInBounds nr nc
    )
    |> Seq.map (fun ((r,c,_), (dr,dc)) ->
        [|0..3|] 
        |> Array.map (fun d -> grid[r+d*dr,c+d*dc])
        |> fun a -> String(a)
    )
    |> Seq.filter(fun s -> s = "XMAS")
    |> Seq.length


solve1 "Day04.txt"

let solve2 input =
    let grid = getInput input 
    grid
    |> Array2DTools.enumArray2d
    |> Seq.filter (fun (r,c,v) -> 
        if v = 'A' then
            if r >= 1 && c >=1 && c <= (grid |> maxC) - 1 && r <= (grid |> maxR) - 1 then
                true
            else
                printfn "Discarding %i,%i" r c
                false
        else
            false
        )
    |> Seq.filter (fun (r,c,_) -> 
        ((grid[r-1,c-1] = 'M' && grid[r+1,c+1] = 'S') || (grid[r-1,c-1] = 'S' && grid[r+1,c+1] = 'M')) &&
        ((grid[r-1,c+1] = 'M' && grid[r+1,c-1] = 'S') || (grid[r-1,c+1] = 'S' && grid[r+1,c-1] = 'M'))
    )
    |> Seq.length

solve2 "Day04.txt"

