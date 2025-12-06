#time "on"
#load "../../Tools.fs"
#r "nuget: NFluent"

open System
open System.IO
open AdventOfCode
open Checked
open NFluent

let getInput name =
    File.ReadLines(getInputPath2025 name)
    |> Seq.map(fun l -> l.Split(" ", StringSplitOptions.RemoveEmptyEntries ||| StringSplitOptions.TrimEntries))

let mult (numbers: int64 seq) =
    let mutable i = 1L
    for n in numbers do
        i <- i * n
    i

let solve1 input =
    let worksheet =
        getInput input
        |> Seq.toArray
        |> Array.transpose
    worksheet
    |> Seq.map (fun problem ->
        let result =
            match problem[problem.Length - 1] with
            | "+" -> problem |> Seq.take (problem.Length - 1) |> Seq.map int64 |> Seq.sum
            | "*" -> problem |> Seq.take (problem.Length - 1) |> Seq.map int64 |> mult
            | x -> failwithf $"invalid op {x}"
        result)
    |> Dump
    |> Seq.sum
    
Check.That(solve1 "Day06_sample1.txt").IsEqualTo(4277556L)

solve1 "Day06.txt"

let solve2 input =
    let input = File.ReadAllLines(getInputPath2025 input)
    let mutable grandTotal = 0L 
    let mutable current = 0L
    let mutable op = (+)
    input
    |> Array.map (fun s -> s.ToCharArray())
    |> Array.transpose
    |> Array.map (fun a -> String(a).Replace(" ", ""))
    |> Array.iter(fun s ->
        match s |> Seq.tryLast with
        | None -> ()
        | Some '*' ->
            grandTotal <- grandTotal + current
            op <- (*)
            current <- s.Substring(0, s.Length - 1) |> int64
        | Some '+' ->
            grandTotal <- grandTotal + current
            op <- (+)
            current <- s.Substring(0, s.Length - 1) |> int64
        | _ ->
            current <- op current (s |> int64)
        )
    grandTotal + current
    
Check.That(solve2 "Day06_sample1.txt").IsEqualTo(3263827L)
solve2 "Day06.txt"
    
    

    