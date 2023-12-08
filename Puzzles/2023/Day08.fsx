
#time
#load "../../Tools.fs"

open System
open System.IO
open AdventOfCode

type Turn = | Left | Right

let charToTurn = function | 'R' -> Turn.Right | 'L' -> Turn.Left | _ -> failwith "not a valid turn"

let inline parseNode (line:string) = line.Substring(0, 3), (line.Substring(7,3), line.Substring(12,3))

let getInput name = 
    let lines = File.ReadAllLines(getInputPath2023 name)
    let turns = lines[0] |> Seq.map charToTurn |> List.ofSeq
    let nodes = lines |> Seq.skip 2 |> Seq.map parseNode |> List.ofSeq
    turns, nodes

let solve1 input =
    getInput input |> Dump

solve1 "Day08_sample.txt"
