#time "on"
#load "../../Tools.fs"
#load "../../Tools/Array2dTools.fs"
#load "../../Tools/AStar.fs"
#load "../../Tools/Distance.fs"
#load "../../Tools/Directions.fs"

open System
open System.IO
open AdventOfCode
open Array2DTools
open FullAStar

let getInput name = 
    File.ReadAllLines(getInputPath2023 name)
    |> Array.map (fun line -> line |> Seq.map (fun c -> (c - '0') |> int) |> Seq.toArray)
    |> array2D

type State = 
    {
        row: int
        col: int
        dir: Direction option
        straight: int
    }

let solve1 input =
    let grid = getInput input

    let getPossibleNextCells s = 
        match s.dir with
        | Some North -> 
            [
                if s.straight < 3 then 
                    yield { row = s.row-1; col =s.col; dir =Some North; straight = s.straight+1 }
                yield { row = s.row; col = s.col-1; dir = Some West; straight = 1 }
                yield { row = s.row; col = s.col+1; dir = Some East; straight = 1 }
            ]
        | Some South -> 
            [
                if s.straight < 3 then 
                    yield { row = s.row+1; col = s.col; dir = Some South; straight =s.straight+1 }
                yield { row = s.row; col = s.col-1; dir = Some West; straight = 1 }
                yield { row = s.row; col = s.col+1; dir = Some East; straight = 1 }
            ]
        | Some East ->
            [
                if s.straight < 3 then 
                    yield { row = s.row; col = s.col+1; dir = Some East; straight = s.straight+1 }
                yield { row = s.row-1; col = s.col; dir = Some North; straight =1 }
                yield { row = s.row+1; col = s.col; dir = Some South; straight =1 }
            ]
        | Some West ->  
            [
                if s.straight < 3 then 
                    yield { row = s.row; col = s.col-1; dir = Some West; straight =s.straight+1 }
                yield { row = s.row-1; col = s.col; dir = Some North; straight =1 }
                yield { row = s.row+1; col = s.col; dir = Some South; straight =1 }
            ]
        | None ->
            [
                yield { row = s.row; col = s.col+1; dir = Some East; straight =1 }
                yield { row = s.row; col = s.col-1; dir = Some West; straight =1 }
                yield { row = s.row-1; col = s.col; dir = Some North; straight =1 }
                yield { row = s.row+1; col = s.col; dir = Some South; straight =1 }
            ]

    let filterInvalidCoords r c = r >= 0 && c >= 0 && r <= (grid |> maxR) && c <= (grid |> maxC)

    let getNeighbors s = 
            getPossibleNextCells s 
            |> Seq.filter (fun s -> filterInvalidCoords s.row s.col)

    let config: Config<State> = {
        fCost = (fun s1 s2 -> manhattanDistance s1.row s1.col s2.row s2.col)
        gCost = (fun s1 s2 -> grid[s2.row, s2.col])
        neighbors = getNeighbors
        maxIterations = None
    }

    let start = { row = 0; col = 0; dir = None; straight = 0 }
    let target = { row = grid |> maxR; col = grid |> maxC; dir = None; straight = 0 }
    match searchWithGoalFunc start target (fun s -> s.row = (grid |> maxR) && s.col = (grid |> maxC)) config with
    | Some cells -> 
        let totalCost = 
            cells 
            |> Seq.filter (fun s -> s.row <> 0 || s.col <> 0)
            |> Seq.map (fun s -> grid[s.row, s.col]) |> Seq.sum
        totalCost
    | None -> 
        failwithf "No solution found"

if solve1 "Day17_sample1.txt" <> 102 then failwithf "Sample 1 not passing"
solve1 "Day17.txt"

