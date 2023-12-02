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
    let isValid game = game |> Seq.forall (fun show -> show.Red <= 12 && show.Green <= 13 && show.Blue <= 14)
    getInput input
    |> Seq.filter (snd >> isValid)
    |> Seq.sumBy fst

let solve2 input =
    getInput input
    |> Seq.map(fun (_, shows) ->
        { 
            Red = shows |> Seq.map (fun s -> s.Red) |> Seq.max
            Green = shows |> Seq.map (fun s -> s.Green) |> Seq.max
            Blue = shows |> Seq.map (fun s -> s.Blue) |> Seq.max
        }
    )
    |> Seq.map (fun s -> (s.Red |> int64) * (s.Green |> int64) * (s.Blue |> int64))
    |> Seq.sum

solve1 "Day02.txt"
solve2 "Day02.txt"