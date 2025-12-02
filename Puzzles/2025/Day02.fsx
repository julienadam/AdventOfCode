#time "on"
#load "../../Tools.fs"
#r "nuget: NFluent"
#r "nuget: FSharp.Collections.ParallelSeq"

open System.IO
open System.Text.RegularExpressions
open AdventOfCode
open Checked
open NFluent
open FSharp.Collections.ParallelSeq

let getInput name =
    File.ReadAllText(getInputPath2025 name)
    |> ssplit ","
    |> Seq.map (fun s -> s |> ssplit2 "-")
    |> Seq.map (fun (a,b) -> a |> int64, b |> int64)
    |> Seq.toList
    
let isInvalidId (i:int64) =
    if i / 100000000000L >= 1L && i / 100000000000L <=9L then
        i % 1000000L = i / 1000000L
    else if i / 1000000000L >= 1L && i / 1000000000L <=9L then
        i % 100000L = i / 100000L
    else if i / 10000000L >= 1L && i / 10000000L <=9L then
        i % 10000L = i / 10000L
    else if i / 100000L >= 1L && i / 100000L <=9L then
        i % 1000L = i / 1000L
    else if i / 1000L >= 1L && i / 1000L <=9L then
        i % 100L = i / 100L
    else if i / 10L >= 1L && i / 10L <=9L then
        i % 10L = i / 10L
    else
        false

let solve1 input =
    getInput input
    |> Seq.collect (fun (a,b) -> [a..b] |> Seq.filter (fun i -> isInvalidId i))
    |> Seq.sum

Check.That(solve1 "Day02_sample1.txt").IsEqualTo(1227775554)

solve1 "Day02.txt"

let regex = Regex("^(\d+)\1+$", RegexOptions.Compiled)

let solve2 input =
    getInput input
    |> PSeq.collect (fun (a,b) ->
        [a..b]
        |> Seq.filter (fun i -> regex.IsMatch($"{i}"))
    )
    |> Seq.sum
    
Check.That(regex.IsMatch("12")).IsFalse()
Check.That(regex.IsMatch("11")).IsTrue()
    
Check.That(regex.IsMatch("2121212121")).IsTrue()
Check.That(regex.IsMatch("824824824")).IsTrue()
Check.That(regex.IsMatch("565656")).IsTrue()
Check.That(regex.IsMatch("38593859")).IsTrue()
Check.That(regex.IsMatch("446446")).IsTrue()
Check.That(regex.IsMatch("222222")).IsTrue()
Check.That(regex.IsMatch("1188511885")).IsTrue()
Check.That(regex.IsMatch("999")).IsTrue()
Check.That(regex.IsMatch("1010")).IsTrue()
Check.That(regex.IsMatch("99")).IsTrue()
Check.That(regex.IsMatch("111")).IsTrue()
Check.That(regex.IsMatch("11")).IsTrue()
Check.That(regex.IsMatch("22")).IsTrue()
    
Check.That(solve2 "Day02_sample1.txt").IsEqualTo(4174379265L)

solve2 "Day02.txt"