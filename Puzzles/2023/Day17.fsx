#time "on"
#load "../../Tools.fs"
#load "../../Tools/Array2dTools.fs"
#load "../../Tools/Distance.fs"
#load "../../Tools/Directions.fs"

open System
open System.IO
open AdventOfCode
open Array2DTools
open System.Collections.Generic

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

    let getNeighbours s = 
            getPossibleNextCells s 
            |> Seq.filter (fun s -> filterInvalidCoords s.row s.col)

    let seen = new HashSet<State>()
    let rec heatlossRec (q:PriorityQueue<State,int>)  =
        match q.TryDequeue() with
        | true, s, hl -> 
            if s.row = (grid |> maxR) && s.col = (grid |> maxC) then
                Some hl
            else
                getNeighbours(s)
                |> Seq.iter(fun n ->
                    if seen.Contains(n) = false then
                        seen.Add(n) |> ignore
                        q.Enqueue(n, hl + grid[n.row, n.col])
                )
                heatlossRec q
        | _ -> 
            None

    let q = new PriorityQueue<State, int>()
    q.Enqueue({ row = 0; col = 0; dir = None; straight = 0 }, 0);
    heatlossRec q

if solve1 "Day17_sample1.txt" <> (Some 102) then failwithf "Sample 1 not passing"
solve1 "Day17.txt"


let solve2 input =
    let grid = getInput input

    let getPossibleNextCells s = 
        match s.dir with
        | Some North -> 
            [
                if s.straight < 10 then 
                    yield { row = s.row-1; col =s.col; dir =Some North; straight = s.straight+1 }
                if s.straight >= 4 then
                    yield { row = s.row; col = s.col-1; dir = Some West; straight = 1 }
                    yield { row = s.row; col = s.col+1; dir = Some East; straight = 1 }
            ]
        | Some South -> 
            [
                if s.straight < 10 then 
                    yield { row = s.row+1; col = s.col; dir = Some South; straight =s.straight+1 }
                if s.straight >= 4 then
                    yield { row = s.row; col = s.col-1; dir = Some West; straight = 1 }
                    yield { row = s.row; col = s.col+1; dir = Some East; straight = 1 }
            ]
        | Some East ->
            [
                if s.straight < 10 then 
                    yield { row = s.row; col = s.col+1; dir = Some East; straight = s.straight+1 }
                if s.straight >= 4 then
                    yield { row = s.row-1; col = s.col; dir = Some North; straight =1 }
                    yield { row = s.row+1; col = s.col; dir = Some South; straight =1 }
            ]
        | Some West ->  
            [
                if s.straight < 10 then 
                    yield { row = s.row; col = s.col-1; dir = Some West; straight =s.straight+1 }
                if s.straight >= 4 then
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

    let getNeighbours s = 
            getPossibleNextCells s 
            |> Seq.filter (fun s -> filterInvalidCoords s.row s.col)

    let seen = new HashSet<State>()
    let rec heatlossRec (q:PriorityQueue<State,int>)  =
        match q.TryDequeue() with
        | true, s, hl -> 
            if s.row = (grid |> maxR) && s.col = (grid |> maxC) && s.straight >= 4 then
                Some hl
            else
                getNeighbours(s)
                |> Seq.iter(fun n ->
                    if seen.Contains(n) = false then
                        seen.Add(n) |> ignore
                        q.Enqueue(n, hl + grid[n.row, n.col])
                )
                heatlossRec q
        | _ -> 
            None

    let q = new PriorityQueue<State, int>()
    q.Enqueue({ row = 0; col = 0; dir = None; straight = 0 }, 0);
    heatlossRec q

if solve2 "Day17_sample1.txt" <> (Some 94) then failwithf "Sample 1 not passing"
if solve2 "Day17_sample2.txt" <> (Some 71) then failwithf "Sample 1 not passing"

solve1 "Day17.txt"
solve2 "Day17.txt"
