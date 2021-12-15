<Query Kind="FSharpProgram" />

let useSample = false;
let dataPath, preambleSize = 
    if useSample then
        Path.Combine(Environment.GetFolderPath(Environment.SpecialFolder.UserProfile), @"CloudStation\Data\AdventOfCode\", "day9_sample.txt"), 5
    else
        Path.Combine(Environment.GetFolderPath(Environment.SpecialFolder.UserProfile), @"CloudStation\Data\AdventOfCode\", "day9.txt"), 25
let input = File.ReadAllLines(dataPath) |> Array.map int64

let allPairsAtDifferentIndices (l1:'a array) (l2: 'a array) = 
    seq {
        for i = 0 to l1.Length - 1 do
            for j = 0 to l2.Length - 1 do
                if i <> j then
                    yield (l1.[i], l2.[j]) 
    }


let s1 = 
    input 
    |> Array.windowed (preambleSize + 1)
    |> Array.find (fun window -> 
        let n = window |> Array.last
        let r = window |> Array.take preambleSize 
        allPairsAtDifferentIndices r r
        |> Seq.map (fun (a,b) -> a + b)
        |> Seq.contains n
        |> not
        )
    |> Seq.last

printfn "Puzzle 1 : %i" s1

let contiguousSumOfS1 = 
    [0..input.Length - 2]
    |> Seq.pick (fun i -> 
        let (a, s1OrMore) =
            [2..input.Length - i]
            |> Seq.map (fun l -> Array.sub input i l)
            |> Seq.map (fun a -> a, a |> Array.sum)
            |> Seq.find (fun (_, s) -> s >= s1)
    
        if s1OrMore = s1 then Some a else None
    )
    
printfn "Puzzle 2 : %i" ((contiguousSumOfS1 |> Array.min) + (contiguousSumOfS1 |> Array.max))