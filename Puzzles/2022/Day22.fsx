#time
#load "../../Tools.fs"
#r "nuget: MathNet.Numerics.FSharp"

open System
open System.IO
open System.Text.RegularExpressions
open System.Collections.Generic
open MathNet.Numerics.LinearAlgebra
open Checked
open AdventOfCode
open AdventOfCode.Array2DTools

let nl = System.Environment.NewLine

type Instr =
    | Forward
    | TurnRight
    | TurnLeft

type MapCell = | Wall | Open | Void

let regex = new Regex"(\d+|R|L)"

let parseInstructions input : seq<Instr>= seq {
    let matches = regex.Matches input
    for m in matches do
        match m.Value with 
        | "R" -> yield TurnRight
        | "L" -> yield TurnLeft
        | s -> yield! [1..(Int32.Parse(s))] |> Seq.map (fun _ -> Forward)
}

module Part1 =
    let wrapOnCol (row, col) (map:MapCell[,]) r =
        match map[r, col] with
        | MapCell.Wall -> Some (row, col)
        | MapCell.Open -> Some (r, col)
        | _ -> None

    let wrapNorth (row, col) (map:MapCell[,]) =
        ([map |> maxR .. -1 .. 0] |> Seq.pick (wrapOnCol (row, col) map)), North

    let wrapSouth (row, col) (map:MapCell[,]) =
        ([0..map |> maxR] |> Seq.pick (wrapOnCol (row, col) map)), South

    let wrapOnRow (row, col) (map:MapCell[,]) c =
        match map[row, c] with 
        | MapCell.Wall -> Some (row, col)
        | MapCell.Open -> Some (row, c)
        | _ -> None

    let wrapEast (row,col) (map:MapCell[,]) =
        ([0..map |> maxC] |> Seq.pick (wrapOnRow (row,col) map)), East

    let wrapWest (row,col) (map:MapCell[,]) =
        ([map |> maxC .. -1 .. 0] |> Seq.pick (wrapOnRow (row,col) map)), West

    let wrap = function | North -> wrapNorth | South -> wrapSouth | East -> wrapEast | West -> wrapWest


    let parseMap input = 
        let lines = input |> ssplit nl 
        let maxRows = lines |> Array.length
        let maxCols = lines |> Seq.map (fun l -> l.Length) |> Seq.max
        let grid = Array2D.create maxRows maxCols MapCell.Void

        for row, line in (lines |> Seq.indexed) do
            for col, c in line |> Seq.indexed do
                match c with
                | '#' -> Array2D.set grid row col MapCell.Wall
                | '.' -> Array2D.set grid row col MapCell.Open
                | _ -> ()
        grid

    type State = {
        row : int; col : int
        direction : Direction
    } with 
        member this.Left () = { this with direction = this.direction.TurnLeft() }
        member this.Right () = { this with direction = this.direction.TurnRight() }

        member private this.updateCoordinates (origR, origC) (newR, newC) direction (map:MapCell[,]) wrapFunc =
            match map[newR, newC] with
            | Open -> (newR, newC), direction
            | Void -> wrapFunc (origR,origC) map
            | Wall -> (origR, origC), direction

        member this.MoveNorth (r,c) (map:MapCell[,]) wrapFunc=
            if r = 0 then wrapFunc (r,c) map
            else this.updateCoordinates (r,c) (r - 1, c) North map wrapFunc

        member this.MoveSouth (r,c) (map:MapCell[,]) wrapFunc =
            if r = maxR map then wrapFunc (r,c) map
            else this.updateCoordinates (r,c) (r + 1, c) South map wrapFunc
            
        member this.MoveEast (r,c) (map:MapCell[,]) wrapFunc=
            if c = maxC map then wrapFunc (r,c) map
            else this.updateCoordinates (r,c) (r, c + 1) East map wrapFunc

        member this.MoveWest (r,c) (map:MapCell[,]) wrapFunc : ((int*int)* Direction)=
            if c = 0 then wrapFunc (r,c) map
            else this.updateCoordinates (r,c) (r, c - 1) West map wrapFunc

        member this.Move moveFunc (map:MapCell[,]) wrapFunc dir =
            (moveFunc (this.row, this.col) map wrapFunc)

        member this.Forward (map:MapCell[,]) wrapper =
            let (r, c), dir = 
                match this.direction with
                | North -> this.Move this.MoveNorth map (wrapper this.direction) this.direction
                | South -> this.Move this.MoveSouth map (wrapper this.direction) this.direction
                | East -> this.Move this.MoveEast map (wrapper this.direction) this.direction
                | West -> this.Move this.MoveWest map (wrapper this.direction) this.direction
            {this with row = r; col = c; direction = dir}

        member this.Print (map:MapCell[,]) =
            for r = 0 to map |> maxR do
                for c = 0 to map |> maxC do
                    if (r,c) = (this.row, this.col) then
                        printf "%c" (match this.direction with | North -> '>' | South -> 'v' | East -> '>' | West -> '<')
                    else
                        printf "%c" (match map[r,c] with | Void -> ' ' | Wall -> '#' | Open -> '.')
                printfn ""

    let getStartingCell (input:string) = 
        (0, input.IndexOf('.'))

    let getInput p =
        let map, instructions = 
            File.ReadAllText(getInputPath2022 p)
            |> ssplit (sprintf "%s%s" nl nl)
            |> tupleize2
        map |> parseMap, map |> getStartingCell, instructions |> parseInstructions |> Seq.toList


    let solve ((map:MapCell[,]),(startRow, startCol),(instructions:Instr list)) wrapper=
        let finalState =
            instructions |> Seq.fold (fun (state:State) instr ->
                //printfn "%A" instr
                let nextState = 
                    match instr with
                    | Forward -> state.Forward map wrapper
                    | TurnLeft -> state.Left()
                    | TurnRight -> state.Right()
                //nextState.Print map
                nextState
            ) { row = startRow; col = startCol; direction = East }
            |> Dump

        let dirScore = match finalState.direction with | East -> 0 | South -> 1 | West -> 2 | North -> 3
        1000 * (finalState.row + 1) + 4 * (finalState.col + 1) + dirScore


    let solve1 input = 
        let m = getInput input
        solve m wrap

// Part1.solve1 "Day22.txt"

module Part2 =

    type PointData = {
        row: int
        col: int
        i: Vector<float>
        j: Vector<float>
        k: Vector<float>
    } with member this.position = this.row, this.col

    let parseMap2 input =
        let lines = input |> ssplit nl
        let grid = new Dictionary<(int*int), bool>()

        for row, line in (lines |> Seq.indexed) do
            for col, c in line |> Seq.indexed |> Seq.filter (fun (_,c) -> not (c = ' ')) do
                let v = match c with | '#' -> false | '.' -> true | _ -> failwithf "Not a valid cell"
                grid.[(row,col)] <- v
        grid

    let getInput p =
        let map, instructions =
            File.ReadAllText(getInputPath2022 p)
            |> ssplit (sprintf "%s%s" nl nl)
            |> tupleize2
        map |> parseMap2, instructions |> parseInstructions |> Seq.toList

    let cross (v1:Vector<float>) (v2:Vector<float>) =
        let v1x, v1y, v1z = v1.[0], v1.[1], v1.[2]
        let v2x, v2y, v2z = v2.[0], v2.[1], v2.[2]
        vector [v1y * v2z - v1z * v2y; v1z * v2x - v1x * v2z; v1x * v2y - v1y * v2x]

    let solve2 input =
        let map, instrs = getInput input
        let side = Math.Sqrt((float) (map.Count / 6)) |> int
        let scaleIJ = side - 1
        let scaleK = side + 1
        let startPos = vector [float(-scaleIJ); float(-scaleIJ); float(-scaleK)]
        let startDir = vector [2.0; 0.0; 0.0]
        let row, col = map.Keys |> Seq.filter (fun (r,_) -> r = 0) |> Seq.minBy (fun (r,c) -> c)
        let start = { row = row; col = col; i = vector [1.0;0.0;0.0]; j = vector [0.0;1.0;0.0] ; k = vector [0.0;0.0;1.0] }

        let todo = System.Collections.Generic.Queue<PointData>([start])
        let visited = System.Collections.Generic.HashSet<int*int>([row, col])
        let points = System.Collections.Generic.Dictionary<Vector<float>, PointData>()

        while todo.Count > 0 do
            let p = todo.Dequeue()
            for x = 0 to side - 1 do
                for y = 0 to side - 1 do
                    let key = (p.i * float(2 * x - scaleIJ)) + (p.j * float(2 * y - scaleIJ)) + (p.k * float(-scaleK))
                    points.[key] <- { p with row = p.row + y; col = p.col + x}

            let neighbors = seq {
                yield { col = p.col + -side ; row = p.row + 0     ; i = cross p.j p.i ; j =  p.j           ; k = cross p.j p.k } // Left
                yield { col = p.col + side  ; row = p.row + 0     ; i = cross p.i p.j ; j =  p.j           ; k = cross p.k p.j } // Right
                yield { col = p.col + 0     ; row = p.row + -side ; i = p.i           ; j =  cross p.j p.i ; k = cross p.k p.i } // Up
                yield { col = p.col + 0     ; row = p.row + side  ; i = p.i           ; j =  cross p.i p.j ; k = cross p.i p.k } // Down
            }

            for next in neighbors do
                let p = next.row, next.col
                if map.ContainsKey(p) && (visited.Contains(p) |> not) then
                    todo.Enqueue(next)
                    visited.Add(p) |> ignore

        points |> Seq.toArray |> Dump |> ignore

        let pos, dir = 
            instrs |> Seq.fold (fun (position, direction) instr ->
                match instr with
                | TurnLeft -> position, (cross direction points.[position].k)
                | TurnRight -> position, (cross points.[position].k direction)
                | Forward -> 
                    let next = position + direction
                    if points.ContainsKey(next) then
                        let pNext = points.[next]
                        if map.[pNext.position] = true then
                            (next, direction)
                        else
                            (position, direction)
                    else
                        let wrapDirection = points[position].k * 2.0 // This is the fun part
                        let wrapPosition = next + wrapDirection
                        if map[points[wrapPosition].position] then
                            (wrapPosition, wrapDirection) 
                        else 
                            (position, direction)

            ) (startPos, startDir)

        pos |> Dump |> ignore
        dir |> Dump |> ignore

        let finalpos = points[pos]
        printfn "Final position : %A" finalpos
        let dirScore = Array.IndexOf([|finalpos.i * 2.0; finalpos.j * 2.0; finalpos.i * -2.0; finalpos.j * -2.0|], dir)
        1000 * (finalpos.row + 1) + 4 * (finalpos.col + 1) + dirScore

let sw = System.Diagnostics.Stopwatch.StartNew()
let result = Part2.solve2 "Day22.txt"
printfn "%i. took %A" result sw.Elapsed


