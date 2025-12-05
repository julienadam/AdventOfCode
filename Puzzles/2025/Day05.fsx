#time "on"
#load "../../Tools.fs"
#r "nuget: NFluent"

open System
open System.IO
open AdventOfCode
open Checked
open NFluent

let getInput name =
    let freshIdRanges, ingredients =
        File.ReadAllText(getInputPath2025 name)
        |> ssplit2 "\n\n"
    
    let freshIdRanges =
        freshIdRanges
        |> ssplit "\n"
        |> Array.map (ssplit2 "-")
        |> Array.map (fun (a,b) -> int64 a, int64 b)
    let ingredients = ingredients |> ssplit "\n" |> Array.map int64
    freshIdRanges, ingredients

let solve1 input =
    let freshIdRanges, ingredients = getInput input // |> Dump
    ingredients
    |> Seq.filter (fun i -> freshIdRanges |> Seq.exists (fun (a,b) -> a <= i && i <= b))
    |> Seq.length

Check.That(solve1 "Day05_sample1.txt").IsEqualTo(3)

solve1 "Day05.txt"

let solve2 input =
    let freshIdRanges, _ = getInput input
    let sortedRanges = freshIdRanges |> Seq.sortBy fst |> Seq.toList
    let rec countFresh ((a,b):int64*int64) remaining totalFresh =
        match remaining with
        | [] -> totalFresh + (b - a + 1L)
        | (c,d)::tail ->
            if c > b then // [a,b] and [c,d] are disjoined -> update total and [c,d]
                countFresh (c,d) tail (totalFresh + (b - a + 1L))
            else 
                if b <= d then // [a,b] and [c,d] overlap -> [a,d]
                    countFresh (a,d) tail totalFresh
                else // [c,d] is totally included in [a,b] -> [a,b]
                    countFresh (a,b) tail totalFresh
         
    countFresh (sortedRanges |> List.head) (sortedRanges |> List.tail) 0L 

Check.That(solve2 "Day05_sample1.txt").IsEqualTo(14)

solve2 "Day05.txt"