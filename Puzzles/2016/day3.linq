<Query Kind="FSharpProgram" />

let fst3 (x,_,_) = x
let snd3 (_,x,_) = x
let trd3 (_,_,x) = x
        
let getInputPath file = Path.Combine(Environment.GetFolderPath(Environment.SpecialFolder.UserProfile), @"CloudStation\Data\AdventOfCode\2016", file)
let path = getInputPath "day3.txt"

let parseLine line = 
    let m = Regex.Match(line, "\s*(?<s1>\d+)\s*(?<s2>\d+)\s*(?<s3>\d+)\s*")
    m.Groups.["s1"].Value |> int, m.Groups.["s2"].Value |> int, m.Groups.["s3"].Value |> int

let verify (a,b,c) = (a+b > c) && (a+c > b) && (b+c > a)

module Puzzle1 =
    let solution () = 
        File.ReadAllLines(path)
        |> Seq.map parseLine
        |> Seq.where verify
        |> Seq.length
        |> Dump
        
printfn "Puzzle 1"
Puzzle1.solution()

module Puzzle2 =
    let solution () =
        let triplets = 
            File.ReadAllLines(path)
            |> Seq.map parseLine
            |> Seq.toList
        
        Seq.concat [triplets |> List.map fst3; triplets |> List.map snd3; triplets |> List.map trd3]
        |> Seq.chunkBySize 3
        |> Seq.map (fun c -> c.[0], c.[1], c.[2])
        //|> Dump
        |> Seq.where verify
        |> Seq.length
        |> Dump
        
printfn "Puzzle 2"
Puzzle2.solution()        