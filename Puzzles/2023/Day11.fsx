
#time
#load "../../Tools.fs"

open System
open System.IO
open AdventOfCode

type Cell =
    | Empty
    | Galaxy

let getInput name = 
    File.ReadAllLines(getInputPath2023 name)
    |> Array.map (fun line -> 
        line |> Seq.map (fun c -> 
            match c with 
            | '.' -> Empty
            | '#' -> Galaxy
            | _ -> failwithf "Not a valid data point %c" c
        )
    )

let solve1 input =
    getInput input |> Dump

solve1 "Day11_sample1.txt"
