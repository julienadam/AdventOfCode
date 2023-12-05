
#time
#load "../../Tools.fs"
#r "nuget: Faqt"


open System
open System.IO
open AdventOfCode
open Ranges

type ConversionResult = 
    | Unchanged of Range64
    | Converted of Range64

type ConversionRange = {
    Source : Range64
    Dest : Range64
}
with 
    member this.Convert(x: int64) = 
        match this.Source.PositionInRange x with
        | Some pos -> this.Dest.Start + pos
        | _ -> x
    member this.Convert(input: Range64) : ConversionResult list=
        if input.Start > this.Source.End then
            // |SS---SE|---|IS---IE|
            [input |> Unchanged]
        else if input.End < this.Source.Start then
            // |IS---IE|---|SS---SE|
            [input |> Unchanged]
        else
            if input.IsInRange(this.Source.Start) then
                if input.IsInRange(this.Source.End) then
                    // |IS---|SS---SE|---IE|
                    [
                        if this.Source.Start > input.Start then
                            yield { Start = input.Start; End = this.Source.Start - 1L} |> Unchanged
                        yield this.Dest |> Converted// Converted range
                        if input.End > this.Source.End then
                            yield { Start = this.Source.End + 1L; End = input.End} |> Unchanged
                    ]
                else
                    // |IS---|SS---IE|---SE|
                    [
                        if input.Start < this.Source.Start then
                            yield { Start = input.Start; End = this.Source.Start - 1L} |> Unchanged
                        yield { Start = this.Dest.Start; End = this.Convert input.End} |> Converted
                    ]
            else if this.Source.IsInRange(input.Start) then
                if this.Source.IsInRange(input.End) then
                    // |SS---|IS---IE|---SE|
                    [
                        { Start = this.Convert(input.Start); End = this.Convert(input.End)} |> Converted
                    ]
                else
                    // |SS---|IS---SE|---IE|
                    [
                        yield { Start = this.Convert(input.Start); End = this.Dest.End} |> Converted
                        if input.End > this.Source.Start then
                            yield { Start = this.Source.End + 1L; End = input.End} |> Unchanged
                    ]
            else
                failwithf "Invalid range intersection"


let intsToConversion (values:int64 array) = 
    { 
        Dest = { Start = values.[0]; End = values.[0]+ values.[2] - 1L }
        Source = { Start = values.[1]; End = values.[1] + values.[2] - 1L }
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
    let conv = ranges |> List.tryFind (fun r -> r.Source.IsInRange value)
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


type State = {
    Todo: Range64 list
    Done: Range64 list
}

let applyRangeConversionsToRange (value:Range64) (ranges: ConversionRange list) =
    let allConverted = 
        ranges |> List.fold (fun state range ->
            // printfn "State %A. Range %A" state range
            if state.Todo.IsEmpty then 
                state
            else 
                let result = state.Todo |> List.collect range.Convert
                let converted, todo = result |> List.partition (fun r -> match r with | Converted _ -> true | _ -> false)
                // printfn "*** result"
                // converted |> Dump|> ignore
                todo |> ignore 
                {
                    Todo = todo |> List.map (fun r -> match r with | Unchanged u -> u | _ -> failwithf "Not an unchanged")
                    Done = 
                        List.concat [
                            state.Done 
                            converted |> List.map (fun r -> match r with | Converted c -> c | _ -> failwithf "Not a converted") 
                        ] 
                }
        ) { Todo = [value]; Done = []}
    // allConverted |> Dump |> ignore
    List.concat [allConverted.Todo; allConverted.Done]

let solve2 input =
    let seeds, maps = getInput input
    let seedRanges = 
        seeds 
        |> Seq.chunkBySize 2
        |> Seq.map (fun r -> {Start = r[0]; End = r[0] + r[1] - 1L })
        |> List.ofSeq
    
    let rec applyStages stage ranges =
        stage |> Dump |> ignore
        ranges |> Dump |> ignore
        match stage with
        | "location" -> ranges
        | intermediate -> 
            let dest, conversionRanges = maps[intermediate]
            let converted = ranges |> List.collect (fun s -> applyRangeConversionsToRange s conversionRanges)
            applyStages dest converted

    applyStages "seed" seedRanges
    |> Seq.map (fun r -> r.Start)
    |> Seq.min

solve2 "Day05_sample.txt"
solve2 "Day05.txt"

open Faqt

let conv = intsToConversion [|50;100;4|]
(conv.Convert {Start = 0; End = 99}).Should().SequenceEqual([{Start = 0; End = 99} |> Unchanged])
(conv.Convert {Start = 0; End = 100}).Should().SequenceEqual([{Start = 0; End = 99} |> Unchanged; {Start = 50; End = 50} |> Converted])
(conv.Convert {Start = 0; End = 101}).Should().SequenceEqual([{Start = 0; End = 99} |> Unchanged; {Start = 50; End = 51} |> Converted])
(conv.Convert {Start = 0; End = 103}).Should().SequenceEqual([{Start = 0; End = 99} |> Unchanged; {Start = 50; End = 53} |> Converted])
(conv.Convert {Start = 0; End = 104}).Should().SequenceEqual([{Start = 0; End = 99} |> Unchanged; {Start = 50; End = 53} |> Converted; {Start = 104; End = 104} |> Unchanged])
(conv.Convert {Start = 0; End = 200}).Should().SequenceEqual([{Start = 0; End = 99} |> Unchanged; {Start = 50; End = 53} |> Converted; {Start = 104; End = 200} |> Unchanged])
(conv.Convert {Start = 100; End = 101}).Should().SequenceEqual([{Start = 50; End = 51} |> Converted])
(conv.Convert {Start = 101; End = 102}).Should().SequenceEqual([{Start = 51; End = 52} |> Converted])
(conv.Convert {Start = 100; End = 103}).Should().SequenceEqual([{Start = 50; End = 53} |> Converted])
(conv.Convert {Start = 100; End = 104}).Should().SequenceEqual([{Start = 50; End = 53} |> Converted; {Start = 104; End = 104} |> Unchanged])
(conv.Convert {Start = 100; End = 200}).Should().SequenceEqual([{Start = 50; End = 53} |> Converted; {Start = 104; End = 200} |> Unchanged])
(conv.Convert {Start = 101; End = 200}).Should().SequenceEqual([{Start = 51; End = 53} |> Converted; {Start = 104; End = 200} |> Unchanged])
(conv.Convert {Start = 103; End = 200}).Should().SequenceEqual([{Start = 53; End = 53} |> Converted; {Start = 104; End = 200} |> Unchanged])
(conv.Convert {Start = 104; End = 200}).Should().SequenceEqual([{Start = 104; End = 200} |> Unchanged])
(conv.Convert {Start = 200; End = 300}).Should().SequenceEqual([{Start = 200; End = 300} |> Unchanged])
