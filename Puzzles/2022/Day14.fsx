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

type Cell =
    | Rock
    | Sand
    | Air

let printGrid (grid: Cell[,]) =
    for r in [0..grid.GetLength(0) - 1] do
        for c in [0..grid.GetLength(1) - 1] do
            printf "%c" (match grid[r,c] with | Rock -> '#' | Sand -> 'O' | _ -> '.' )
        printfn ""
    ()

let solve1 (traces: ((int*int) array)seq) =
    let cols = traces |> Seq.collect id |> Seq.map fst
    let rows = traces |> Seq.collect id |> Seq.map snd
    let minC = cols |> Seq.min
    let maxC = cols |> Seq.max
    let maxR = rows |> Seq.max

    let grid = Array2D.init (maxR + 1) (maxC - minC + 1) (fun _ _ -> Air)

    traces |> Seq.iter (fun points -> 
        points 
        |> Seq.windowed 2
        |> Seq.iter (fun ps ->
            let (c1, r1), (c2, r2) = ps |> Seq.toArray |> tupleize2
            for c in [(min c1 c2)..(max c1 c2)] do
                let translatedC = c - minC
                for r in [(min r1 r2)..(max r1 r2)] do
                    Array2D.set grid r translatedC Rock
        ))

    grid

getInput "Day14_sample1.txt"
|> solve1
|> printGrid