﻿#time
#load "../../Tools.fsx"

open System
open System.IO
open System.Text.RegularExpressions
open Checked
open Tools

let nl = System.Environment.NewLine

type Instr =
    | Forward of int
    | TurnRight
    | TurnLeft

type MapCell = | Wall | Open | Void

type State = {
    row : int; col : int
    direction : Direction
} with 
    member this.Left () = { this with direction = this.direction.TurnLeft() }
    member this.Right () = { this with direction = this.direction.TurnRight() }
    member this.maxR map = (map |> Array2D.length1) - 1
    member this.maxC map = (map |> Array2D.length2) - 1
    
    member private this.wrapOnCol (row, col) (map:MapCell[,]) r =
        match map[r, col] with
        | MapCell.Wall -> Some (row, col)
        | MapCell.Open -> Some (r, col)
        | _ -> None

    member private this.wrapNorth (row, col) (map:MapCell[,]) =
        [map |> this.maxR .. -1 .. 0] |> Seq.pick (this.wrapOnCol (row, col) map)

    member private this.wrapSouth (row, col) (map:MapCell[,]) =
        [0..map |> this.maxR] |> Seq.pick (this.wrapOnCol (row, col) map)

    member private this.wrapOnRow (row, col) (map:MapCell[,]) c =
        match map[row, c] with 
        | MapCell.Wall -> Some (row, col)
        | MapCell.Open -> Some (row, c)
        | _ -> None

    member private this.wrapEast (row,col) (map:MapCell[,]) =
        [0..map |> this.maxC] |> Seq.pick (this.wrapOnRow (row,col) map)

    member private this.wrapWest (row,col) (map:MapCell[,]) =
        [map |> this.maxC .. -1 .. 0] |> Seq.pick (this.wrapOnRow (row,col) map)

    member this.MoveNorth (r,c) (map:MapCell[,]) wrapFunc=
        if r = 0 then
            wrapFunc (r,c) map
        else
            match map[r - 1, c] with
            | Open -> (r - 1, c)
            | Void -> wrapFunc (r,c) map
            | Wall -> (r, c)
                
    member this.MoveSouth (r,c) (map:MapCell[,]) wrapFunc =
        if r = this.maxR map then
            wrapFunc (r,c) map
        else
            match map[r + 1, c] with
            | Open -> (r + 1, c)
            | Void -> wrapFunc (r,c) map
            | Wall -> (r, c)
            
    member this.MoveEast (r,c) (map:MapCell[,]) wrapFunc=
        if c = this.maxC map then
            wrapFunc (r,c) map
        else
            match map[r, c + 1] with
            | Open -> (r, c + 1)
            | Void -> wrapFunc (r,c) map
            | Wall -> (r, c)

    member this.MoveWest (r,c) (map:MapCell[,]) wrapFunc=
        if c = 0 then
            wrapFunc (r,c) map
        else
            match map[r, c - 1] with
            | Open -> (r, c - 1)
            | Void -> wrapFunc (r,c) map
            | Wall -> (r, c)

    member this.Move x moveFunc (map:MapCell[,]) wrapFunc = [1..x] |> Seq.fold (fun (r,c) _ -> moveFunc (r,c) map wrapFunc) (this.row, this.col)

    member this.Forward x (map:MapCell[,]) =
        let (r,c) = 
            match this.direction with
            | North -> this.Move x this.MoveNorth map this.wrapNorth
            | South -> this.Move x this.MoveSouth map this.wrapSouth
            | East -> this.Move x this.MoveEast map this.wrapEast
            | West -> this.Move x this.MoveWest map this.wrapWest
        {this with row = r; col = c}

    member this.Print (map:MapCell[,]) =
        for r = 0 to map |> this.maxR do
            for c = 0 to map |> this.maxC do
                if (r,c) = (this.row, this.col) then
                    printf "%c" (match this.direction with | North -> '>' | South -> 'v' | East -> '>' | West -> '<')
                else
                    printf "%c" (match map[r,c] with | Void -> ' ' | Wall -> '#' | Open -> '.')
            printfn ""

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

let parseMap input = 
    let lines = input |> ssplit nl 
    let maxRows = lines |> Array.length
    let maxCols = lines |> Seq.map (fun l -> l.Length) |> Seq.max
    printfn "rows : %i cols : %i" maxRows maxCols
    let grid = Array2D.create maxRows maxCols MapCell.Void

    for row, line in (lines |> Seq.indexed) do
        for col, c in line |> Seq.indexed do
            match c with
            | '#' -> Array2D.set grid row col MapCell.Wall
            | '.' -> Array2D.set grid row col MapCell.Open
            | _ -> ()
    grid

let getStartingCell (input:string) = 
    (0, input.IndexOf('.'))

let getInput p =
    let map, instructions = 
        File.ReadAllText(getInputPath2022 p)
        |> ssplit (sprintf "%s%s" nl nl)
        |> tupleize2
    map |> parseMap, map |> getStartingCell, instructions |> parseInstructions |> Seq.toList


let solve1 ((map:MapCell[,]),(startRow, startCol),(instructions:Instr list)) =
    let finalState =
        instructions |> Seq.fold (fun (state:State) instr ->
            printfn "%A" instr
            let nextState = 
                match instr with
                | Forward x -> state.Forward x map
                | TurnLeft -> state.Left()
                | TurnRight -> state.Right()
            // nextState.Print map
            nextState
        ) { row = startRow; col = startCol; direction = East }
        |> Dump

    let dirScore = match finalState.direction with | North -> 3 | South -> 1 | East -> 0 | West -> 2
    1000 * (finalState.row + 1) + 4 * (finalState.col + 1) + dirScore


getInput "Day22.txt"
//|> Dump
|> solve1

// Part 2
// Need to include direction in wrap function
// Refactor Part1 to take a wrap func in the move func
// DONE : Need to extract the cube size from the map, just width / 2