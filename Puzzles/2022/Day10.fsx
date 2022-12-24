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
    
    [20; 60; 100; 140; 180; 220] |> Seq.map (fun i -> i * (cycles[i] |> snd)) |> Seq.sum

getInput "Day10.txt"
|> solve1
|> Dump


let solve2 (instructions: Instruction seq) =
    let cycles = 
        execute instructions 
        |> Seq.toArray
        |> Seq.skip 1

    let crt : seq<char> = 
        cycles |> Seq.map (fun (cycle, x) ->
            let pos = (cycle - 2) % 40
            if pos >= x - 1 && pos <= x + 1 then
                '\u2588'
            else
                ' ')
    crt 
    |> Seq.chunkBySize(40)
    |> Seq.iter (fun c ->
        printfn "%s" (new String(c))
    )


getInput "Day10.txt"
|> solve2
