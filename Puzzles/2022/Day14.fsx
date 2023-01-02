#load "../../Tools.fs"
open System
open System.IO
open AdventOfCode

let mapTrace line =
    line
    |> ssplit " -> "
    |> Array.map (split2 ',')
    |> Array.map (fun (s1,s2) -> Int32.Parse(s1), Int32.Parse(s2))

type Cell = | Rock | Sand
type Cells = System.Collections.Generic.Dictionary<(int*int), Cell>
type Grid = {
    cells: Cells
    minC: int
    maxC: int
    maxR: int
}

let mapGrid (traces: ((int*int) array)seq) =
    let grid = new Cells()
    traces |> Seq.iter (fun points -> 
        points 
        |> Seq.windowed 2
        |> Seq.iter (fun ps ->
            let (c1, r1), (c2, r2) = ps |> Seq.toArray |> tupleize2
            for c in [(min c1 c2)..(max c1 c2)] do
                for r in [(min r1 r2)..(max r1 r2)] do
                    if grid.ContainsKey (r,c) then
                        grid.[r,c] = Rock |> ignore
                    else
                        grid.Add((r,c), Rock) |> ignore
        ))

    let minC = grid |> Seq.map (fun kvp -> kvp.Key |> snd) |> Seq.min
    let maxC = grid |> Seq.map (fun kvp -> kvp.Key |> snd) |> Seq.max
    let maxR = grid |> Seq.map (fun kvp -> kvp.Key |> fst) |> Seq.max
    { cells = grid; minC = minC; maxC = maxC; maxR = maxR }

let getInput p = 
    File.ReadAllLines(getInputPath2022 p)
    |> Seq.map mapTrace
    |> mapGrid

let printGrid (grid: Grid) =
    for r in [0..grid.maxR] do
        for c in [grid.minC..grid.maxC] do
            if grid.cells.ContainsKey (r,c) then
                printf "%c" (match grid.cells[r,c] with | Rock -> '#' | Sand -> 'O' )
            else
                printf "%c" '.'
        printfn ""
    grid

let rec sandFall (r,c) (grid:Grid) =
    if r >= grid.maxR then
        printfn "Fell off the bottom %i %i" r grid.maxR
        printGrid grid |> ignore
        None
    else
        match grid.cells.TryGetValue((r+1,c)) with
        | false, _ -> sandFall (r+1, c) grid
        | _ ->
            if c-1 < 0 then
                printfn "Fell off the left"
                printGrid grid |> ignore
                None
            else
                match grid.cells.TryGetValue((r+1, c-1)) with
                | false, _ -> sandFall (r+1, c-1) grid
                | _ -> 
                    if c+1 > grid.maxC then
                        printfn "Fell off the right"
                        printGrid grid |> ignore
                        None
                    else
                        match grid.cells.TryGetValue((r+1, c+1)) with
                        | false, _ -> sandFall (r+1, c+1) grid
                        | _ -> 
                            grid.cells.Add((r,c),Sand) |> ignore
                            Some grid

let origin = 0, 500

let letItFlow grid = seq {
    let mutable counter = 0
    while true do
        let result = grid |> sandFall origin
        if result.IsSome
            then counter <- counter + 1
        else
            printfn "%i" counter

        yield result
}

let solve1 grid =
    letItFlow grid
    |> Seq.find (fun a -> a.IsNone)

//getInput "Day14.txt"
//|> solve1

let rec fillWithSand (row,col) (grid:Grid) numGrains =
    if row = grid.maxR + 1 then
        // Hit the infinite bottom
        grid.cells.Add((row,col),Sand) |> ignore
        fillWithSand origin grid (numGrains + 1)
    else
        // testing down
        match grid.cells.TryGetValue((row+1,col)) with
        | false, _ -> fillWithSand (row+1, col) grid numGrains
        | _ ->
            // testing down left
            match grid.cells.TryGetValue((row+1, col-1)) with
            | false, _ -> fillWithSand (row+1, col-1) grid numGrains
            | _ -> 
                // testing down right
                match grid.cells.TryGetValue((row+1, col+1)) with
                | false, _ -> fillWithSand (row+1, col+1) grid numGrains
                | _ -> 
                    if (row,col) = origin then
                        // grain just rested at origin, stop
                        printGrid grid |> ignore
                        numGrains + 1
                    else
                        // grain is at rest, next please!
                        grid.cells.Add((row,col),Sand) |> ignore
                        fillWithSand origin grid (numGrains + 1)


let solve2 grid = fillWithSand origin grid 0

getInput "Day14.txt"
|> solve2