open System.Collections.Generic
open System.Text

#time "on"
#load "../../Tools.fs"

open System
open System.IO
open AdventOfCode
open Checked

let getInput name = 
    let (vars, ops) = 
        File.ReadAllText(getInputPath2024 name).Replace("\r", "")
        |> ssplit "\n\n"
        |> tupleize2

    let varList = vars |> ssplit "\n" |> Seq.map (fun line -> line |> ssplit ":" |> tupleize2)
    let opList = ops |> ssplit "\n" |> Seq.map (fun line -> line |> ssplit " " |> Array.filter (fun s -> s <> "->"))
    varList |> Seq.map (fun (v,n) -> v, n |> int), opList |> Seq.map tupleize4

type Operation = string*string*string*string

let solve1 input =
    let vars, ops = getInput input
    let varValuesMap = new Dictionary<string, int>(vars |> Seq.map (fun (a,b) -> new KeyValuePair<string, int>(a,b)))
    let opsQueue = new Queue<Operation>(ops)

    while opsQueue.Count > 0 do
        let (left, op, right, target) = opsQueue.Dequeue()
        match varValuesMap.TryGetValue(left), varValuesMap.TryGetValue(right) with
        | (true, l), (true, r) ->
            let result = 
                match op with
                | "AND" -> l &&& r
                | "OR" -> l ||| r
                | "XOR" -> l ^^^ r
                | x -> failwithf "Invalid op %s" x
            if varValuesMap.TryAdd(target, result) = false then failwithf "Overwrite"
        | _ -> opsQueue.Enqueue((left, op, right, target))

    let bits =
        varValuesMap 
        |> Seq.filter(fun kvp -> kvp.Key.StartsWith "z")
        |> Seq.map(fun kvp -> kvp.Key.Substring(1) |> int, kvp.Value)
        |> Seq.sortByDescending fst
        |> Seq.map snd

    let sb = new StringBuilder()
    for b in bits do
        sb.Append(b) |> ignore
    Int64.Parse(sb.ToString(), Globalization.NumberStyles.BinaryNumber)

    //let mutable shift = 0
    //let mutable result = 0L
    //for b in bits do
    //    if b = 1 then
    //        let r = b <<< shift
    //        result <- result ||| r
    //    else if b = 0 then ()
    //    else failwithf ("Invalid bit")
    //    shift <- shift + 1
    //result

#r "nuget: NFluent"
open NFluent

Check.That(solve1 "Day24_sample1.txt").Equals(4)
Check.That(solve1 "Day24_sample2.txt").Equals(2024)
solve1 "Day24.txt"
