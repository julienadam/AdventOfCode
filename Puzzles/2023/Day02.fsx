#time
#load "../../Tools.fs"

open System
open System.IO
open AdventOfCode

type Show = {
    Red : int
    Green : int
    Blue : int
}

let parse line =
    let _, contents = line |> ssplit2 ": "
    contents 
    |> ssplit "; " 
    |> Array.map (fun show -> 
        show 
        |> ssplit ", "
        |> Array.fold 
            (fun show str ->
                let qty, color = str |> split2 ' '
                match color with 
                | "red" -> { show with Red = qty |> int }
                | "green" -> { show with Green = qty |> int }
                | "blue" -> { show with Blue = qty |> int }
                | _ -> failwithf "invalid color %s" color
            )
            { Red = 0; Green = 0; Blue = 0 }
    )

let getInput p = 
    File.ReadAllLines(getInputPath2023 p) 
    |> Array.mapi (fun i l -> i + 1, parse l)


let solve1 input =
    getInput input
    |> Seq.filter(fun (_, shows) ->
        shows |> Seq.forall (fun show -> show.Red <= 12 && show.Green <= 13 && show.Blue <= 14)
    )
    |> Seq.sumBy fst

solve1 "Day02.txt"