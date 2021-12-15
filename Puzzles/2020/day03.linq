<Query Kind="FSharpProgram" />

let inputPath = @"C:\Users\Arcodia\CloudStation\Data\AdventOfCode\day3.txt"

let lines = File.ReadAllLines(inputPath)
let l = lines.[0].Length
// l |> Dump

let hasTree x y =
    if y - 1 > lines.Length then 
        false
    else
        lines.[y - 1].[(x - 1) % l] = '#'

let s1 () =

    [1..lines.Length - 1] 
    |> Seq.map (fun i -> ((3*i) + 1, i + 1), hasTree (3*i + 1) (i + 1))
    |> Seq.where snd
    |> Seq.length
    |> Dump
    
let s2 () =

    let countTrees (xo, yo) =
        [1..lines.Length - 1] 
        |> Seq.map (fun i -> 
            let x = (xo * i) + 1
            let y = (yo * i) + 1
            (x, y), hasTree x y)
        |> Seq.where snd
        |> Seq.length
        

    let slopes = [ (1,1); (3, 1); (5, 1); (7, 1); (1, 2)]
    slopes |> Seq.map countTrees |> Seq.fold (fun s a -> s * a) 1
    
    
s2() |> Dump