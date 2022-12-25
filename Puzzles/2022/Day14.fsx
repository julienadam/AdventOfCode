#load "../../Tools.fsx"
open System
open System.IO
open Tools

let mapTrace line =
    line
    |> ssplit " -> "
    |> Array.map (split2 ',')
    |> Array.map (fun (s1,s2) -> Int32.Parse(s1), Int32.Parse(s2))

let origin = 500,0

let getInput p = 
    File.ReadAllLines(getInputPath2022 p)
    |> Seq.map mapTrace

let solve1 (traces: ((int*int) array)seq) =
    let cols = traces |> Seq.collect id |> Seq.map fst
    let rows = traces |> Seq.collect id |> Seq.map snd

    let minC = cols |> Seq.min
    let maxC = cols |> Seq.max

    let minR = 0 // rows |> Seq.min
    let maxR = rows |> Seq.max

    minC, maxC, minR, maxR

getInput "Day14_sample1.txt"
|> solve1