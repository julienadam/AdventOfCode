#time
#load "../../Tools.fs"
#load "../../Tools/SparseGrid.fs"
#load "../../Tools/Directions.fs"
open System
open System.IO
open System.Collections.Generic
open Checked
open AdventOfCode
open AdventOfCode.SparseGrid

let parseMap input =
    let grid = new Dictionary<(int*int), bool>()
    for row, line in (input |> Seq.indexed) do
        for col, c in line |> Seq.indexed |> Seq.filter (fun (_,c) -> not (c = ' ')) do
            let v = match c with | '#' -> true | '.' -> false | _ -> failwithf "Not a valid cell"
            grid.[(row,col)] <- v
    grid

let getInput p =
    File.ReadAllLines(getInputPath2022 p)
    |> parseMap


let hasElf pos (map:IDictionary<(int*int), bool>) =
    match map.TryGetValue(pos) with
    | true, e -> e
    | _ -> false

let round (directions:Direction list) (map:IDictionary<(int*int), bool>) =
    let potentiallyMovingElves = 
        map 
        |> Seq.filter kvpValue 
        |> Seq.map kvpKey
        |> Seq.filter (fun pos -> 
            // Filter elves that have at least one neighbour
            getAdjacentWithDiagonals pos map |> Seq.exists (fun (_,_,v) -> v.IsSome && v.Value)
        )

    let proposedMoves = 
        potentiallyMovingElves |> Seq.choose (fun (r,c) ->
            directions |> Seq.tryPick (fun dir ->
                let proposed = 
                    match dir with
                    | North -> 
                        if (map |> hasElf (r-1, c-1) |> not) && (map |> hasElf (r-1, c) |> not) && (map |> hasElf (r-1, c+1) |> not) 
                        then (r-1, c) |> Some else None
                    | South -> 
                        if (map |> hasElf (r+1, c-1) |> not) && (map |> hasElf (r+1, c) |> not) && (map |> hasElf (r+1, c+1) |> not) 
                        then (r+1, c) |> Some else None
                    | West -> 
                        if (map |> hasElf (r-1, c-1) |> not) && (map |> hasElf (r, c-1) |> not) && (map |> hasElf (r+1, c-1) |> not)
                        then (r, c-1) |> Some else None
                    | East -> 
                        if (map |> hasElf (r-1, c+1) |> not) && (map |> hasElf (r, c+1) |> not) && (map |> hasElf (r+1, c+1) |> not)
                        then (r, c+1) |> Some else None
                proposed |> Option.bind (fun p -> Some ((r,c), p))
            )
        )
    let validMoves = 
        proposedMoves 
        |> Seq.groupBy snd 
        |> Seq.choose (fun (_, values) -> values |> Seq.tryExactlyOne)
        |> Seq.toList
    validMoves 
    |> Seq.iter (fun ((elfPos), targetPos) -> 
        map.Remove(elfPos) |> ignore
        map.[targetPos] <- true
    )
    
    map, List.append directions.Tail [directions.Head], validMoves.IsEmpty

let printer v = match v with | Some v when v -> '#' | _ -> '.'

let solve1 rounds map =
    //printGrid printer map |> ignore
    let finalMap, _ =
        [1 .. rounds] |> Seq.fold (fun (map, directions) i ->
            //printfn "Round #%i, directions are %A" i directions
            let map2, dir, _ = round directions map
            //printGrid printer map2 |> ignore
            map2, dir
        ) (map, [North; South; West; East])

    Seq.allPairs [finalMap |> minR .. finalMap |> maxR] [finalMap |> minC .. finalMap |> maxC]
    |> Seq.filter (fun pos -> match finalMap.TryGetValue(pos) with | true, true -> false | _ -> true)
    |> Seq.length

getInput "Day23_sample1.txt"
|> solve1 10

getInput "Day23.txt"
|> solve1 10

let solve2 map =
    let rec solveRec roundNumber directions =
        let _, dir, noMoves = round directions map
        if noMoves then
            roundNumber
        else
            solveRec (roundNumber + 1) dir

    solveRec 1 [North; South; West; East]

getInput "Day23.txt"
|> solve2