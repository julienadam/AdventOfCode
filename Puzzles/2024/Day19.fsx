#time "on"
#load "../../Tools.fs"

open System
open System.IO
open AdventOfCode
open System.Text.RegularExpressions
open Checked

let getInput name = 
    let patterns, designs = 
        File.ReadAllText(getInputPath2024 name).Replace("\r", "")
        |> ssplitNoEmpty "\n\n" |> tupleize2
    patterns |> ssplitNoEmpty ", ", designs |> ssplitNoEmpty "\n"

let solve1 input =
    let patterns, designs = getInput input
    let reTowels = sprintf("^(%s)+$") (String.Join("|", patterns))
    let regex = new Regex(reTowels, RegexOptions.Compiled)
    designs |> Seq.filter (fun d -> regex.IsMatch(d)) |> Seq.length

solve1 "Day19_sample1.txt"
solve1 "Day19.txt"

open System.Collections.Generic

let solve2 input =
    let patterns, designs = getInput input
    let countArrangements design =
        let memo = new Dictionary<int, int64>()
        let rec countArrangementsRec index (design:Memory<char>) =
            if index = design.Length then 1L
            else
                let matchingPatterns = patterns |> Seq.filter (fun p -> design.Slice(index).Span.StartsWith(p))
                if matchingPatterns |> Seq.isEmpty then 0L
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

solve2 "Day19_sample1.txt"
solve2 "Day19.txt"
