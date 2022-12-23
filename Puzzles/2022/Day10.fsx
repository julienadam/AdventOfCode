#load "../../Tools.fsx"

open System
open System.IO
open Tools

type Instruction =
    | Noop
    | Addvx of int

let mapLine line = 
    match line with
    | "noop" -> Noop
    | s -> Addvx (Int32.Parse(s.Split(' ').[1]))

let getInput p = File.ReadAllLines(getInputPath2022 p) |> Seq.map mapLine

getInput "Day10.txt" |> Dump