#time "on"
#load "../../Tools.fs"

open System
open System.IO
open AdventOfCode
open Checked

type Schematic = | Lock | Key

let getInput name = 
    File.ReadAllText(getInputPath2024 name).Replace("\r", "")
    |> ssplit "\n\n"
    |> Array.map (fun s-> s |> ssplit "\n")
    |> Array.map (fun s -> if s[0] = "#####" then Lock, s else Key, s)

let lockToHeightMap (lock:string array) =
    [0..4] |> Seq.map (fun c ->
        let h = [1..6] |> Seq.find(fun r -> lock[r][c] = '.')
        h - 1
    ) |> Seq.toArray

let keyToHeightMap (lock:string array) =
    [0..4] 
    |> Seq.map (fun c -> [0..5] |> Seq.rev |> Seq.find(fun r -> lock[r][c] = '.'))
    |> Seq.toArray

let solve1 input =
    let keysAndLocks = getInput input
    let keys = keysAndLocks |> Seq.filter (fun (t, _) -> t = Key) |> Seq.map snd |> Seq.toArray
    let locks = keysAndLocks |> Array.filter (fun (t, _) -> t = Lock) |> Seq.map snd |> Seq.toArray
    let lockHeights = locks |> Array.map lockToHeightMap |> Dump
    let keyHeights = keys |> Seq.map keyToHeightMap

    Seq.allPairs lockHeights keyHeights
    |> Seq.filter (fun (lh, kh) ->
        kh[0] >= lh[0] && kh[1] >= lh[1] && kh[2] >= lh[2] && kh[3] >= lh[3] && kh[4] >= lh[4]
    )
    |> Seq.length

solve1 "Day25_sample1.txt"
solve1 "Day25.txt"
