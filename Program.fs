open System
open System.IO
open AdventOfCode
open System.Collections.Generic
open FSharp.Collections.ParallelSeq
open Checked

let getInput name = 
    let patterns, designs = 
        File.ReadAllText(getInputPath2024 name).Replace("\r", "")
        |> ssplitNoEmpty "\n\n" |> tupleize2
    patterns |> ssplitNoEmpty ", ", designs |> ssplitNoEmpty "\n"

let solveCore (patterns:string array) (designs:string array) =
    let countArrangements design =
        let memo = new Dictionary<int, int64>()
        let rec countArrangementsRec index (design:Memory<char>) =
            if index = design.Length then 1L
            else
                let matchingPatterns = patterns |> Seq.filter (fun p -> design.Slice(index).Span.StartsWith(p))
                if matchingPatterns |> Seq.isEmpty then 
                    memo.TryAdd(index, 0L) |> ignore
                    0L
                else
                    // use the memoized solution if it exists, or compute and memoize if not
                    match memo.TryGetValue(index) with
                    | true, memoized -> memoized
                    | false, _ ->
                        let sum = 
                            matchingPatterns
                            |> Seq.map (fun p -> countArrangementsRec (index + p.Length) design)
                            |> Seq.sum
                        memo.TryAdd(index, sum) |> ignore
                        sum
        countArrangementsRec 0 design

    designs |> Seq.sumBy (fun d -> countArrangements (new Memory<char>(d.ToCharArray())))


let solveCoreString (patterns:string array) (designs:string array) =
    let countArrangements design =
        let memo = new Dictionary<int, int64>()
        let rec countArrangementsRec index (design:string)  =
            if index = design.Length then 1L
            else
                let matchingPatterns = patterns |> Seq.filter (fun p -> design.Substring(index).StartsWith(p))
                if matchingPatterns |> Seq.isEmpty then 
                    memo.TryAdd(index, 0L) |> ignore
                    0L
                else
                    // use the memoized solution if it exists, or compute and memoize if not
                    match memo.TryGetValue(index) with
                    | true, memoized -> memoized
                    | false, _ ->
                        let sum = 
                            matchingPatterns
                            |> Seq.map (fun p -> countArrangementsRec (index + p.Length) design)
                            |> Seq.sum
                        memo.TryAdd(index, sum) |> ignore
                        sum
        countArrangementsRec 0 design

    designs |> Seq.sumBy (fun d -> countArrangements d)

let solveCoreParallel (patterns:string array) (designs:string array) =
    let countArrangements design =
        let memo = new Dictionary<int, int64>()
        let rec countArrangementsRec index (design:Memory<char>) =
            if index = design.Length then 1L
            else
                let matchingPatterns = patterns |> Seq.filter (fun p -> design.Slice(index).Span.StartsWith(p))
                if matchingPatterns |> Seq.isEmpty then 
                    memo.TryAdd(index, 0L) |> ignore
                    0L
                else
                    // use the memoized solution if it exists, or compute and memoize if not
                    match memo.TryGetValue(index) with
                    | true, memoized -> memoized
                    | false, _ ->
                        let sum = 
                            matchingPatterns
                            |> Seq.map (fun p -> countArrangementsRec (index + p.Length) design)
                            |> Seq.sum
                        memo.TryAdd(index, sum) |> ignore
                        sum
        countArrangementsRec 0 design

    designs |> PSeq.sumBy (fun d -> countArrangements (new Memory<char>(d.ToCharArray())))

open BenchmarkDotNet.Running
open BenchmarkDotNet.Attributes

[<ShortRunJob>]
[<MemoryDiagnoser>]
type public Bench () =
    let patterns, designs = getInput "Day19.txt"

    [<Benchmark>]
    member public this.SolveWithString() = solveCoreString patterns designs

    [<Benchmark>]
    member public this.SolveWithMemory() = solveCore patterns designs

    [<Benchmark>]
    member public this.SolveWithMemoryParallel() = solveCoreParallel patterns designs

BenchmarkRunner.Run<Bench>() |> ignore