﻿open System
open AdventOfCode

type Color = | Red | Green | Blue

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

parse "Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green" |> Dump