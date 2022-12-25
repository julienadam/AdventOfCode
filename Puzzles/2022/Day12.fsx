open System.Collections.Generic
open Tools.AStar


#load "../../Tools.fsx"

open System
open System.IO
open Tools

let getInput p = 
    let mutable start = None
    let mutable target = None

    let grid = 
        File.ReadAllLines(getInputPath2022 p) 
        |> Seq.mapi (fun row s -> 
            s.ToCharArray()
            |> Seq.mapi (fun col c -> 
                let elevChar = 
                    match c with
                    | 'E' -> 
                        target <- (row, col) |> Some
                        'z'
                    | 'S' ->
                        start <- (row, col) |> Some
                        'a'
                    | _ -> c

                (int) (elevChar - 'a')
                )
            ) 
        |> Seq.toArray 
        |> array2D

    if start.IsNone || target.IsNone then
        failwith "Could not find start or target coordinates"

    grid, start.Value, target.Value


let grid, start, target = getInput "Day12_sample1.txt"

let getValidMoves (row, col) = 
    let current = grid.[row, col]
    grid 
    |> Array2DTools.getAdjacent row col
    |> Seq.where (fun (_,_,v) -> v <= current + 1)
    |> Seq.map (fun (row, col, _) -> row, col)

let config : FullAStar.Config<int * int> = {
    neighbours = getValidMoves
    gCost = fun _ _ -> 1.0
    fCost = fun (r1, c1) (r2, c2) -> manhattanDistance r1 c1 r2 c2
    maxIterations = None
}

let shortestPath = FullAStar.search start target config