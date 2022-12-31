#time
#load "../../Tools.fsx"

open System
open System.IO
open System.Text.RegularExpressions
open Checked
open Tools

let nl = System.Environment.NewLine

type Instr =
    | Forward of int
    | Right
    | Left

type MapCell =
    | Wall
    | Open


let regex = new Regex"(\d+|R|L)"

let parseInstructions input : seq<Instr>= seq {
    let matches = regex.Matches input
    for m in matches do
        yield 
            match m.Value with 
            | "R" -> Right
            | "L" -> Left
            | s -> Forward (Int32.Parse(s))
}

let parseMap input = seq {
    let lines = input |> ssplit nl |> Seq.indexed
    for row, line in lines do
        for col, c in line |> Seq.indexed do
            match c with
            | '#' -> yield (row,col), MapCell.Wall
            | '.' -> yield (row,col), MapCell.Open
            | _ -> ()
}

let getStartingCell (input:string) = 
    (0, input.IndexOf('.'))

let getInput p =
    let map, instructions = 
        File.ReadAllText(getInputPath2022 p)
        |> ssplit (sprintf "%s%s" nl nl)
        |> tupleize2
    map |> parseMap |> Map.ofSeq, map |> getStartingCell, instructions |> parseInstructions |> Seq.toList

getInput "Day22_sample1.txt"
|> Dump