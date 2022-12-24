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

// getInput "Day10_sample1.txt" |> Dump

let execute (instructions: Instruction seq) = seq {
    let mutable cycle = 1
    let mutable x = 1
    yield cycle, x
    for inst in instructions do
        match inst with
        | Noop -> 
            cycle <- cycle + 1
            yield cycle, x
        | Addvx value -> 
            cycle <- cycle + 1
            yield cycle, x
            cycle <- cycle + 1
            yield cycle, x
            x <- x + value
}

let solve1 (instructions: Instruction seq) =
    let cycles = 
        execute instructions 
        |> Seq.toArray
        //|> Array.map (fun (c,x) -> 
        //    printfn "%i\t%i" c x
        //    c,x)
    
    [20; 60; 100; 140; 180; 220] |> Seq.map (fun i -> i * (cycles[i] |> snd)) |> Seq.sum

getInput "Day10.txt"
|> solve1
|> Dump