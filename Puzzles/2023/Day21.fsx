#time "on"
#load "../../Tools.fs"
#load "../../Tools/Array2DTools.fs"
#r "nuget: NFluent"

open System
open System.IO
open AdventOfCode
open System.Collections.Generic
open NFluent

let solve steps name print =
    let lines = File.ReadAllLines(getInputPath2023 name)
    let grid = new Dictionary<(int*int), unit>()
    lines |> Seq.iteri (fun r l -> l |> Seq.iteri (fun c ch -> if ch = '#' then grid.Add((r,c), ()) |> ignore))
    let side = lines[0].Length
    let startPos = (side / 2, side / 2)
    let inline normalizeCoords x = if x < 0 then side - ((abs x) % side) else x % side

    let getAdjacentCoords row col = [((row - 1), col); ((row + 1), col); (row, (col - 1)); (row, (col + 1))]

    let mutable inRange = new HashSet<int*int>()
    let mutable toVisit = new HashSet<int*int>()
    toVisit.Add(startPos) |> ignore
    
    [1..steps] |> Seq.iter (fun s ->
        let visitedThisRound = new HashSet<int*int>()
        for (r,c) in toVisit do
            getAdjacentCoords r c
            |> Seq.filter (fun (r, c) -> 
                grid.ContainsKey(r |> normalizeCoords, c |> normalizeCoords) = false)
            |> Seq.iter (fun (r,c) -> visitedThisRound.Add(r,c) |> ignore)
        ()

        if s = steps then
            inRange <- visitedThisRound
        else
            toVisit <- visitedThisRound
    )

    if print then
        let minR = inRange |> Seq.map fst |> Seq.min
        let maxR = inRange |> Seq.map fst |> Seq.max
        let minC = inRange |> Seq.map snd |> Seq.min
        let maxC = inRange |> Seq.map snd |> Seq.max

        for r = minR - 5 to maxR + 5 do
            for c = minC - 5 to maxC + 5 do
                if inRange.Contains(r,c) then
                    printf "O"
                else
                    if (r,c) = startPos then 
                        printf "S"
                    else
                        if grid.ContainsKey(r |> normalizeCoords, c |> normalizeCoords) then
                            printf "#"
                        else 
                            printf "."
            printfn ""

    inRange.Count

Check.That(solve 6 "Day21_sample1.txt" false).IsEqualTo(16)
Check.That(solve 10 "Day21_sample1.txt" false).IsEqualTo(50)
Check.That(solve 50 "Day21_sample1.txt" false).IsEqualTo(1594)

solve 64 "Day21.txt" true

// side is 131
// mid point is 65
// number of steps is 202300 * 131 + 65
// so the idea is to solve for 65, 131 + 65 and 131 *2 + 65, put that in a quadratic fitter
// and solve away

let a0 = solve 65 "Day21.txt" false
let a1 = solve (131+65) "Day21.txt" false
let a2 = solve (131+131+65) "Day21.txt" false

#r "nuget: MathNet.Numerics"
open MathNet.Numerics
let fit = Fit.Polynomial([|0;1;2|],[|a0;a1;a2|], 2)
let f x = fit[0] + fit[1]*x + fit[2]*x*x
(f 202300) |> int64
