open System.Diagnostics


#load "../../Tools.fs"

open System
open System.IO
open AdventOfCode

let path = getInputPath "day14.txt"
//let path = getInputPath "day14_sample1.txt"

let input = File.ReadLines(path) 
let polymerTemplate = input |> Seq.head

let mapPairInsertionline (l:string) =
    let split = l.Split(" -> ")
    split.[0], split.[1]

let pairInsertions = input |> Seq.skip 2 |> Seq.map mapPairInsertionline |> Map.ofSeq

module Part1 =

    let rec applyInsertions (polymer:string) (insertions:Map<string, string>) position =
        match position with
        | p when p = polymer.Length - 1 -> polymer
        | _ -> 
            let pair = polymer.Substring(position, 2)
            match insertions |> Map.tryFind pair with
            | Some i -> 
                let pWithInsert = polymer.Insert(position + 1, i)
                applyInsertions pWithInsert insertions (position + 2)
            | None ->
                applyInsertions polymer insertions (position + 1)

    let Solve() = 
        let modifiedPolymer = 
            [1..10] 
            |> Seq.fold (fun p _ -> applyInsertions p pairInsertions 0) polymerTemplate 
            |> Dump
            
        let sorted = 
            modifiedPolymer 
            |> Seq.groupBy id 
            |> Seq.map (fun (_, g) -> g |> Seq.length)
            |> Seq.sort
        
        ((sorted |> Seq.last) - (sorted |> Seq.head)) |> Dump
        
Part1.Solve()

module Part2 =

    let evolve (pairCounts:(string * int64) seq) (insertions:Map<string,string>) =
        // For each pair, inserted the char and split into 2 pairs
        // The count for the new pairs is the count of the initial pair
        let evolved = 
            pairCounts |> Seq.collect (fun (pair, count) ->
                let c = insertions.[pair]
                let before = pair.Substring(0,1) + c
                let after = c + pair.Substring(1,1)
                [before, count; after, count])
                
        // Once this is done, pairs might appear multiple times
        // So we'll group them and calculate the sum
        evolved 
        |> Seq.groupBy fst 
        |> Seq.map (fun (k,g) -> k, g |> Seq.map snd |> Seq.sum)

    let computeLetterTotals (pairCounts:(string * int64) seq) (initialPolymer:string) =
        // Consider only the first letter of each pair, since the last char in a pair is the
        // First one in another pair
        // The only exception will be a +1 adjustment for the last char of the polymer
        pairCounts 
        |> Seq.map (fun (pair, count) -> pair.[0], count)
        |> Seq.groupBy fst
        |> Seq.map (fun (k, g) -> 
            let sum = g |> Seq.map snd |> Seq.sum
            if k = initialPolymer.[initialPolymer.Length - 1] then
                // Add one to the last character of the initial polymer chain since it is fixed
                k, sum + 1L
            else
                k, sum)

    let Solve() = 
        // Group chars by pairs and calculate the initial count for each pair
        let initialState = 
            polymerTemplate 
            |> Seq.windowed 2 
            |> Seq.map (fun w -> String(w))
            |> Seq.groupBy id
            |> Seq.map (fun (k,g) -> k, g |> Seq.length |> int64)
        
        let sw = Stopwatch.StartNew()
        
        // Do 40 iterations
        let finalState = 
            [1..40] 
            |> Seq.fold (fun state _ -> evolve state pairInsertions) initialState
            
        // Calculate the
        let totals = computeLetterTotals finalState polymerTemplate
        let sorted = totals |> Seq.map snd |> Seq.sort
        printfn "Elapsed %A" sw.Elapsed
        ((sorted |> Seq.last) - (sorted |> Seq.head)) |> Dump
        ()
        
Part2.Solve()
