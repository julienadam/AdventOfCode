<Query Kind="FSharpProgram" />

let inputPath = @"C:\Users\Arcodia\CloudStation\Data\AdventOfCode\day1.txt"
let input = File.ReadAllLines(inputPath) |> Seq.map int

// Star 1
let s1 () = 
    let allPairs xs ys = seq { for x in xs do for y in ys -> x, y  }
    
    let x, y = 
        allPairs input input
        |> Seq.find (fun (i1, i2) -> i1+i2 = 2020)
    
    (x * y)

s1() |> Dump

// Star 2

let allTriplets xs ys zs = seq { for x in xs do for y in ys do for z in zs -> x, y, z  }

let s2 () =

    let x, y, z = 
        allTriplets input input input
        |> Seq.find (fun (i1, i2, i3) -> i1 + i2 + i3 = 2020)
    
    (x * y * z)
    
s2() |> Dump