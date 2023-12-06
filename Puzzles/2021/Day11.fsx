#load "../../Tools.fs"
#load "../../Tools/Array2DTools.fs"

open System
open System.IO
open AdventOfCode
open AdventOfCode.Array2DTools

let path = getInputPath "day11.txt"
//let path = getInputPath "day11_sample1.txt"

let input = 
    File.ReadAllLines(path) 
    |> Array.map (fun s -> s |> Seq.map (fun c -> (int c) - (int '0')) |> Seq.toArray) 
    |> array2D 

let inline step grid = grid |> Array2D.map (fun i -> i + 1)

let flash i j grid =
    Array2D.set grid i j 0
    grid
    |> getAdjacentWithDiagonals i j 
    |> Seq.iter (fun (x,y,v) -> Array2D.set grid x y (v + 1))

let rec updateGridRec (flashed:Set<int*int>) (grid:int[,]) =
    let flashing = grid |> enumArray2d |> Seq.filter (fun (i,j,v) -> v > 9 && (flashed |> Set.contains (i,j) |> not)) |> Seq.toList
    match flashing with
    | [] -> flashed
    | toFlash -> 
        toFlash |> Seq.iter (fun (x, y, _) -> flash x y grid)
        let justFlashed = toFlash |> Seq.map (fun (x,y,_) -> (x,y)) |> Set.ofSeq
        updateGridRec (Set.union flashed justFlashed) grid
        
    
let updateGrid grid = 
    // Recursively update grid until no more octopii have enough energy for a flash
    let flashed = updateGridRec Set.empty grid
    // Set all flashed cells to 0
    flashed |> Seq.iter (fun (i,j) -> Array2D.set grid i j 0)
    grid, flashed |> Set.count

module Part1 =
    let Solve() =
    
        [1..100] |> Seq.fold (fun (gridAtStep, totalFlashes) _ -> 
            let grid, flashed = gridAtStep |> step |> updateGrid
            grid, (totalFlashes + flashed)
            ) (input, 0) |> Dump
        
        ()
        
Part1.Solve()

module Part2 =
    
    let isSynchedFlash grid = grid |> enumArray2d |> Seq.forall (fun (_,_,v) -> v = 0)
    
    let rec solveRec grid stepNo =
        let (g, _) = grid |> step |> updateGrid 
        if g |> isSynchedFlash then
            stepNo
        else
            solveRec g (stepNo + 1)
        

    let Solve () =
        solveRec input 1 |> Dump
        ()
        
Part2.Solve()
