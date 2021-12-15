#load "../../Tools.fsx"
open System
open System.IO
open Tools

type Operation =
| Forward of int
| Up of int
| Down of int

let mapLine (l: string) = 
    let split = l.Split(" ")
    let v = split.[1] |> Int32.Parse
    match split.[0] with
    | "forward" -> Forward v
    | "up" -> Up v
    | "down" -> Down v
    | x -> failwithf "Invalid instruction %s" x
    
let getInput f = File.ReadAllLines(getInputPath f) |> Seq.map mapLine

module Part1 =

    let Solve file =
        let finalHorizPos, finalDepth = 
            getInput file
            |> Seq.fold (fun (h, d) instr ->
                match instr with
                | Forward x -> h + x, d
                | Up x -> h, d - x
                | Down x -> h, d + x) (0, 0)
        (finalHorizPos * finalDepth) |> Dump

Part1.Solve "Day02.txt"

module Part2 =

    let Solve file =
        let finalHorizPos, finalDepth, _ = 
            getInput file
            |> Seq.fold (fun (h, d, a) instr ->
                match instr with
                | Down x -> h, d, a + x
                | Up x -> h, d, a - x
                | Forward x -> h + x, d + a * x, a) 
                (0, 0, 0)
        (finalHorizPos * finalDepth) |> Dump

Part2.Solve "Day02.txt"
