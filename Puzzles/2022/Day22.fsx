﻿open System.Collections.Generic


#time
#load "../../Tools.fsx"

open System
open System.IO
open System.Text.RegularExpressions
open Checked
open Tools
open Tools.Array2DTools

let nl = System.Environment.NewLine

type Instr =
    | Forward of int
    | TurnRight
    | TurnLeft

type MapCell = | Wall | Open | Void

let regex = new Regex"(\d+|R|L)"

let parseInstructions input : seq<Instr>= seq {
    let matches = regex.Matches input
    for m in matches do
        yield 
            match m.Value with 
            | "R" -> TurnRight
            | "L" -> TurnLeft
            | s -> Forward (Int32.Parse(s))
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

        member this.MoveWest (r,c) (map:MapCell[,]) wrapFunc =
            if c = 0 then wrapFunc (r,c) map
            else this.updateCoordinates (r,c) (r, c - 1) West map wrapFunc

        member this.Move x moveFunc (map:MapCell[,]) wrapFunc dir = 
            [1..x] 
            |> Seq.fold (fun ((r,c), _) _ -> moveFunc (r,c) map wrapFunc) ((this.row, this.col), dir)

        member this.Forward x (map:MapCell[,]) wrapper =
            let ((r,c), dir) = 
                match this.direction with
                | North -> this.Move x this.MoveNorth map (wrapper this.direction) this.direction
                | South -> this.Move x this.MoveSouth map (wrapper this.direction) this.direction
                | East -> this.Move x this.MoveEast map (wrapper this.direction) this.direction
                | West -> this.Move x this.MoveWest map (wrapper this.direction) this.direction
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
                    | Forward x -> state.Forward x map wrapper
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

    let parseMap2 input =
        let lines = input |> ssplit nl
        let grid = new Dictionary<(int*int), bool>()

        for row, line in (lines |> Seq.indexed) do
            for col, c in line |> Seq.indexed |> Seq.filter (fun (_,c) -> not (c = ' ')) do
                let v = match c with | '#' -> true | '.' -> false | _ -> failwithf "Not a valid cell"
                grid.[(row,col)] <- v
        grid

    let getInput p =
        let map, instructions =
            File.ReadAllText(getInputPath2022 p)
            |> ssplit (sprintf "%s%s" nl nl)
            |> tupleize2
        map |> parseMap2, instructions |> parseInstructions |> Seq.toList

    let solve2 input =
        let map, instrs = getInput input
        let totalChars = map.Count
        let side = Math.Sqrt((float) (totalChars / 6)) |> int
        printfn "side length %i" side
        
Part2.solve2 "Day22_sample1.txt"

