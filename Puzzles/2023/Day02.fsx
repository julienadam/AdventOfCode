
#time
#load "../../Tools.fs"

open System
open System.IO
open AdventOfCode

type Color = | Red | Green | Blue

let getInput p = File.ReadAllLines(getInputPath2023 p)

let parse line =
    let inline parseColor color = match color with | "red" -> Color.Red | "green" -> Color.Green | "blue" -> Color.Blue | _ -> failwithf "invalid color %s" color
    let _, contents = line |> ssplit2 ": "
    contents 
    |> ssplit "; " 
    |> Array.map (fun show -> 
        show 
        |> ssplit ", "
        |> Array.map (split2 ' ')
        |> Array.map (fun (qty, color) ->
            qty |> int, parseColor color
        )
    )

getInput "Day02.txt"
|> Array.map parse
|> Dump
