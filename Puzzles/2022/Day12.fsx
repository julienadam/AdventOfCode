
#load "../../Tools.fs"

open System.IO
open AdventOfCode

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


let solve1 (grid : int[,], start, target) =
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

    let shortestPath = 
        FullAStar.search start target config 

    match shortestPath with 
    | Some i -> Some ((i |> Seq.length) - 1)
    | _ -> None

//getInput "Day12.txt"
//|> solve1 
//|> Dump

let solve2 (grid : int[,], _, target: int*int) =
    let startingPoints =
        grid 
        |> Array2DTools.enumArray2d
        |> Seq.filter (fun (_, _, v) -> v = 0)
        |> Seq.map (fun (r,c,_) -> r, c)

    startingPoints
    |> Seq.choose (fun s -> solve1 (grid, s, target))
    |> Seq.min


open System.Diagnostics
let sw = Stopwatch.StartNew()

getInput "Day12.txt"
|> solve2
|> Dump

printfn "Solved in %A" sw.Elapsed