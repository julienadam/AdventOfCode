
#time
#load "../../Tools.fs"

open System
open System.IO
open AdventOfCode
open Ranges

type ConversionRange = {
    SourceRange : Range64
    DestStart : int64
}
with 
    member this.Convert(x: int64) = 
        match this.SourceRange.PositionInRange x with
        | Some pos -> this.DestStart + pos
        | _ -> x

let intsToConversion (values:int64 array) = 
    { 
        DestStart = values.[0]
        SourceRange = { Start = values.[1]; End = values.[1] + values.[2] - 1L }
    }

let readMapBlock block =
    let mapLines = block |> ssplit "\r\n"
    let (mapFrom, mapTo) = (mapLines[0] |> ssplit " ")[0] |> ssplit2 "-to-"
    let conversionRanges  = 
        mapLines
        |> Seq.skip 1
        |> Seq.map splitSpaceIntList64
        |> Seq.map intsToConversion
        |> List.ofSeq
    mapFrom, (mapTo, conversionRanges)

let getInput name = 
    let blocks = 
        File.ReadAllText(getInputPath2023 name)
        |> ssplit "\r\n\r\n"

    let seeds = blocks.[0].Substring(7).Trim() |> splitSpaceIntList64
    let maps = 
        blocks
        |> Seq.skip 1
        |> Seq.map readMapBlock
        |> Map.ofSeq
    seeds, maps

let inline applyRangeConversions value (ranges: ConversionRange list) =
    let conv = ranges |> List.tryFind (fun r -> r.SourceRange.IsInRange value)
    match conv with
    | Some c -> c.Convert value
    | _ -> value    //|> Seq.fold (fun v range -> range.Convert v) value

let solve1 input =
    let seeds, maps = getInput input
    let rec applyStages stage values =
        // stage |> Dump |> ignore
        // values |> Dump |> ignore
        match stage with
        | "location" -> values
        | intermediate -> 
            let dest, ranges = maps[intermediate]
            let converted = values |> Array.map (fun s -> applyRangeConversions s ranges)
            applyStages dest converted

    applyStages "seed" seeds |> Dump |> Seq.min

solve1 "Day05_sample.txt"
solve1 "Day05.txt"


// Tests
// let conv = intsToConversion [| 50;98;2 |] |> Dump
// conv.Convert 97
// conv.Convert 98
// conv.Convert 99
// conv.Convert 100

// let ranges = [ 
//     intsToConversion [| 50;98;2 |]
//     intsToConversion [| 52;50;48 |]
// ]

// applyRangeConversions 79 ranges
// applyRangeConversions 14 ranges
// applyRangeConversions 55 ranges
// applyRangeConversions 13 ranges

// let ranges = [ 
//     intsToConversion [|49;53;8|]
//     intsToConversion [|0;11;42|]
//     intsToConversion [|42;0;7|]
//     intsToConversion [|57;7;4|]
// ]

// ranges |> List.map (fun conv -> conv.SourceRange.IsInRange 53)


// applyRangeConversions 53 ranges