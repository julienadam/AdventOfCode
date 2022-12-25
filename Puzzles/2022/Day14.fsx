#load "../../Tools.fsx"
open System
open System.IO
open Tools

let mapTrace line =
    line
    |> ssplit " -> "
    |> Array.map (split2 ',')
    |> Array.map (fun (s1,s2) -> Int32.Parse(s1), Int32.Parse(s2))

type Cell =
    | Rock
    | Sand
    | Air

let mapGrid (traces: ((int*int) array)seq) =
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

    grid, minC

let getInput p = 
    File.ReadAllLines(getInputPath2022 p)
    |> Seq.map mapTrace
    |> mapGrid

let printGrid (grid: Cell[,]) =
    for r in [0..grid.GetLength(0) - 1] do
        for c in [0..grid.GetLength(1) - 1] do
            printf "%c" (match grid[r,c] with | Rock -> '#' | Sand -> 'O' | _ -> '.' )
        printfn ""
    grid

let rec sandFall (r,c) minC (grid:Cell[,]) =
    let rMax = grid.GetLength(0) - 1
    let tc = c - minC
    
    if r >= rMax then
        printfn "Fell off the bottom %i %i" r rMax
        printGrid grid |> ignore
        None
    else
        match grid.[r+1,tc] with
        | Air -> sandFall (r+1, c) minC grid
        | _ ->
            if tc-1 < 0 then
                printfn "Fell off the left"
                printGrid grid |> ignore
                None
            else
                match grid.[r+1, tc-1] with
                | Air -> sandFall (r+1, c-1) minC grid
                | _ -> 
                    if tc+1 > grid.GetLength(1) - 1 then
                        printfn "Fell off the right"
                        printGrid grid |> ignore
                        None
                    else
                        match grid.[r+1, tc+1] with
                        | Air -> sandFall (r+1, c+1) minC grid
                        | _ -> 
                            Array2D.set grid r tc Sand
                            Some grid


let letItFlow grid minC = seq {
    let origin = 0, 500
    let mutable counter = 0
    while true do
        let result = grid |> sandFall origin minC
        if result.IsSome
            then counter <- counter + 1
        else    
            printfn "%i" counter

        yield result
}

let solve1 (grid,minC) =
    letItFlow grid minC
    |> Seq.find (fun a -> a.IsNone)

getInput "Day14.txt"
|> solve1
