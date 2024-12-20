open System.Threading

#time "on"
#load "../../Tools.fs"
#load "../../Tools/Array2DTools.fs"
#load "../../Tools/Distance.fs"
#r "nuget: FSharp.Collections.ParallelSeq"

open System.IO
open AdventOfCode
open Checked
open System.Collections.Generic
open FSharp.Collections.ParallelSeq

let getInput name = File.ReadAllLines(getInputPath2024 name) |> array2D

let solve cheatDuration input minCheat = 
    let grid = getInput input 
    let (sr,sc, _) = grid |> Array2DTools.findi (fun _ _ v -> v = 'S')
    let (er,ec, _) = grid |> Array2DTools.findi (fun _ _ v -> v = 'E')

    // it's not a maze, so there's only a single path
    // walk it backwards so we have a list of points and the index is the distance to the goal
    let path = new List<int*int>()
    let dirs = [(0,1);(0,-1);(1,0);(-1,0)]
    let (++) (r1,c1) (r2,c2) = (r1+r2), (c1+c2)
    let rec walk pos prev =
        path.Add(pos)
        if pos = (sr,sc) then ()
        else
            let next = dirs |> Seq.map(fun d -> pos++d) |> Seq.find (fun (r,c) -> grid[r,c] <> '#' && (r,c) <> prev)
            walk next pos

    walk (er,ec) (er,ec)

    let cheats = ref 0

    path |> PSeq.iteri (fun index a ->
        // Find shortcuts (they are at least more than minCheat cells away)
        for cheatTo = index + minCheat to path.Count - 1 do 
            let b = path[cheatTo]
            let md = manhattanDistPoints a b
            if cheatTo - index - md >= minCheat && md <= cheatDuration then
                Interlocked.Increment(cheats) |> ignore
    )
    cheats.Value

let solve1 = solve 2

solve1 "Day20.txt" 100

let solve2 = solve 20

solve2 "Day20.txt" 100