#time
#load "../../Tools.fs"

open System
open System.IO
open Checked
open AdventOfCode

let parseMap (input:string array) =
    let width = input.[0].Length
    let height = input.Length

    let blizzards = 
        seq {
            for row, line in (input |> Seq.indexed) do
                for col, c in line |> Seq.indexed |> Seq.filter (fun (_,c) -> not (c = ' ')) do
                    match c with
                    | '>' -> yield row, col, East
                    | '^' -> yield row, col, North
                    | '<' -> yield row, col, West
                    | 'v' -> yield row, col, South
                    | _ -> ()
        } |> Seq.toList
    blizzards, height, width

let getInput p =
    File.ReadAllLines(getInputPath2022 p)
    |> parseMap

let solve1 blizzards height width =
    let entrance = 0, 1
    let exit = height - 1, width - 1
    printfn "Entrance : %A Exit : %A" entrance exit

let evolve (blizzards, height, width) =
    blizzards |> List.map (fun (r, c, b) ->
        match b with
        | North ->
            (if r = 1 then height - 2 else r - 1), c, North
        | South ->
            (if r = height - 2 then 1 else r + 1), c, South
        | East ->
            r, (if c = width - 2 then 1 else c + 1), East
        | West ->
            r, (if c = 1 then width - 2 else c - 1), West
    ), height, width

let print (blizzards, height, width) =
    for r = 0 to height - 1 do
        for c = 0 to width - 1 do
            if r = 0 || r = height - 1 then
                printf "#" 
            else if c = 0 || c = width - 1 then
                printf "#"
            else 
                match blizzards |> List.filter (fun (br,bc,dir) -> r = br && c = bc) |> List.map (fun (_,_,d) -> d) with
                | [] -> printf "."
                | [North] -> printf "^"
                | [South] -> printf "v"
                | [East] -> printf ">"
                | [West] -> printf "<"
                | [_;_] -> printf "2"
                | [_;_;_] -> printf "3"
                | [_;_;_;_] -> printf "4"
                | _ -> failwithf "not possible ?"
        printfn ""
    (blizzards, height, width)

getInput "Day24_sample2.txt"
|> print
|> evolve
|> print
|> evolve
|> print
|> evolve
|> print
|> evolve
|> print
|> evolve
|> print