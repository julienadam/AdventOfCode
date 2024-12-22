open System.Collections.Generic

#time "on"
#load "../../Tools.fs"
#load "../../Tools/SeqEx.fs"
#load "../../Tools/Array2DTools.fs"

open System
open System.IO
open AdventOfCode
open Array2DTools
open Checked

let getInput name = File.ReadAllLines(getInputPath2024 name)

let manhattanPath (ra,ca) (rb,cb) = seq {
    if rb > ra then
        for _ = ra + 1 to rb do yield 'v'
    else if rb < ra then
        for _ = rb + 1 to ra do yield '^'
    if cb > ca then
        for _ = ca + 1 to cb do yield '>'
    else if cb < ca then
        for _ = cb + 1 to ca do yield '<'
}

let allManhattanPaths pA pB =
    let dirs = manhattanPath pA pB |> Seq.toList
    SeqEx.permutations dirs.Length dirs |> Seq.distinct

let numPadMap = 
    [ 
        "789"; 
        "456"; 
        "123"; 
        ".0A"
    ] |> array2D |> filteri (fun _ _ v -> v <> '.') |> Seq.map (fun (r,c,v) -> v, (r,c)) |> Map.ofSeq

let dirMap = [('^', (-1,0)); ('v', (1,0)); ('>', (0,1)); ('<', (0,-1))] |> Map.ofSeq
let (++) (r1,c1) (r2,c2) = (r1+r2, c1+c2)

// Check that we don't go into the forbidden spot along the way
let hitsForbiddenSpot (start:int*int) forbidden (instructions:char seq) =
    let mutable spot = start
    instructions |> Seq.exists (fun i ->
        spot <- spot ++ dirMap[i]
        spot = forbidden
    )

let allNumPadPaths a b =
    let pA = numPadMap[a]
    let pB = numPadMap[b]
    allManhattanPaths pA pB
    |> Seq.filter (fun instructions -> not(hitsForbiddenSpot pA (3,0) instructions))
    |> Seq.toList

// allNumPadPaths '0' '1'

let dirPadMap = [ ".^A"; "<v>" ] |> array2D |> filteri (fun _ _ v -> v <> '.') |> Seq.map (fun (r,c,v) -> v, (r,c)) |> Map.ofSeq

let allDirPadPaths a b =
    let pA = dirPadMap[a]
    let pB = dirPadMap[b]
    allManhattanPaths pA pB
    |> Seq.filter (fun instructions -> not(hitsForbiddenSpot pA (0,0) instructions))
    |> Seq.toList

// allDirPadPaths '<' 'A'

let numPadMovesMap =
    let npChars = "0123456789A".ToCharArray()
    Array.allPairs npChars npChars
    |> Seq.map (fun (a,b) -> 
        if a = b then 
            (a,b), ["A"]
        else
            (a,b), allNumPadPaths a b |> List.map (fun s -> new String(s |> List.toArray) + "A")
    )
    |> Map.ofSeq

let dirPadMovesMap = 
    let dpChars = "<>^vA".ToCharArray() 
    Array.allPairs dpChars dpChars
    |> Seq.map (fun (a,b) -> 
        if a = b then 
            (a,b), ["A"]
        else
            (a,b), allDirPadPaths a b |> List.map (fun s -> new String(s |> List.toArray) + "A")
    )
    |> Map.ofSeq

let getShortest code maxDepth =
    let memo = new Dictionary<string*int*int, int64>()

    let rec getShortestRec input depth =
        match memo.TryGetValue((input, maxDepth, depth)) with
        | true, r -> r
        | false, _ ->
            let mutable prevKey = 'A'
            let mutable currLength = 0L
            for key in input do
                let movesMap = if depth = 0 then numPadMovesMap else dirPadMovesMap
                let moves = movesMap[(prevKey, key)]

                if (depth = maxDepth) then
                    currLength <- currLength + (moves[0].Length |> int64)
                else
                    let minLength =
                        moves
                        |> Seq.map(fun path -> getShortestRec path (depth + 1)) 
                        |> Seq.min
                    currLength <- currLength + minLength
                prevKey <- key;
            memo[(input, maxDepth, depth)] <- currLength;
            currLength
    getShortestRec code 0

let solve1 input = 
    getInput input 
    |> Seq.map (fun code -> (code[0..2] |> int64 ), (getShortest code 2))
    |> Seq.sumBy (fun (a,b) -> a * b)

// solve1 "Day21_sample1.txt"
solve1 "Day21.txt"

let solve2 input = 
    getInput input 
    |> Seq.map (fun code -> (code[0..2] |> int64 ), (getShortest code 25))
    |> Seq.sumBy (fun (a,b) -> a * b)

solve2 "Day21.txt"