#load "../../Tools.fsx"
#time "on"

open System
open System.IO
open Tools

// Part 1 : 79997391969649
// Part 2 : 16931171414113

let triplets = 
    getInputPath "Day24.txt"
    |> File.ReadAllLines 
    |> Array.chunkBySize 18
    |> Array.map (fun c -> c.[4].Substring(6) |> int, c.[5].Substring(6) |> int, c.[15].Substring(6) |> int) 
    
let range = [0..9] |> List.rev

let rec solveLevel level (zl:int seq) (result:Map<int, Map<int, (int*int) list>>) =
    let A,B,C = triplets.[level]
    let solutions = 
        seq {
            for w in range do
                for z in zl do
                    for a in [0..A-1] do
                        let pz = z * A + a
                        if pz % 26 + B = w && pz / A = z then
                            yield pz, (w, z)
                        let pz = Math.Round(((z - w - C) / 26 * A + a) |> decimal) |> int
                        if pz % 26 + B <> w && pz / A * 26 + w + C = z then
                            yield pz, (w, z)
        } 
        |> Seq.groupBy fst
        |> Seq.map (fun (k, v) -> k, v |> Seq.map snd |> Seq.toList)
        |> Map.ofSeq

    let nextResult = result |> Map.add level solutions

    if level > 0 then
        solveLevel (level-1) solutions.Keys nextResult
    else
        nextResult

let result = solveLevel (14-1) [0] Map.empty

let rec solveRec1 i z sol =
    if i = 14 then
        Some (String.Join("", sol |> List.map string |> Seq.toArray))
    else
        result.[i].[z] 
        |> List.sortDescending
        |> Seq.tryPick (fun (w, nz) -> solveRec1 (i+1) nz (List.append sol [w]))

let rec solveRec2 i z sol =
    if i = 14 then
        Some (String.Join("", sol |> List.map (fun i -> (i + 1) |> string) |> Seq.toArray))
    else
        result.[i].[z] 
        |> List.sort 
        |> List.tryPick (fun (w, nz) -> solveRec2 (i+1) nz (List.append sol [w]))

let solve1() = solveRec1 0 0 []
let solve2() = solveRec2 0 0 []

solve1()
solve2()