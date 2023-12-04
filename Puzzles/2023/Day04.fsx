
#time
#load "../../Tools.fs"

open System
open System.IO
open AdventOfCode

let parseLine (line:string) =
    let parseIntList = ssplitNoEmpty " " >> Seq.map trim >> Seq.map int
    let numbers = line |> ssplit2 ": " |> snd
    let l, r = numbers |>  ssplit2 " | "
    l |> parseIntList |> Set.ofSeq, r |> parseIntList |> List.ofSeq

let getInput p = 
    File.ReadAllLines(getInputPath2023 p)
    |> Array.mapi (fun i l -> i + 1, parseLine l)

let cardScore (winningNumbers: Set<int>) (myNumbers: int list) =
    let wins = 
        myNumbers 
        |> Seq.filter (fun n -> winningNumbers |> Set.contains n) 
        |> Seq.length
    if wins > 0 then pow2 (wins - 1) else 0

let solve1 input = 
    getInput input
    |> Seq.map (fun (_, (winners, numbers)) -> 
        cardScore winners numbers
    )
    |> Seq.toList
    |> Seq.sum

solve1 "Day04.txt"
