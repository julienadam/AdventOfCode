<Query Kind="FSharpProgram" />

let path = 
    Path.Combine(
        Environment.GetFolderPath(Environment.SpecialFolder.UserProfile), 
        "CloudStation\Data\AdventOfCode\day10.txt")
let input = File.ReadAllLines(path) |> Array.map int |> Array.toList

let _1s, _3s = 
    0 :: input 
    |> List.sort
    |> List.pairwise
    |> List.fold (fun (ones, threes) (a,b) -> 
        match b-a with 
        | 1 -> (ones + 1, threes) 
        | 3 -> (ones, threes + 1) 
        | d -> failwithf "Diff is neither 1 nor 3 but %i" d) (0,0)

(_1s * (_3s + 1)) |> Dump

let sorted = 0 :: (input |> List.sort)
let memo = new Dictionary<int, int64>()
    
let rec walk input =
    match input with
    | hd :: tl -> 
        match memo.TryGetValue hd with
        | true, memoed -> memoed
        | _ -> 
            match (tl |> List.takeWhile (fun i -> i <= hd + 3)) with
            | [] -> 1L
            | adapters ->
                let a = 
                    [0..adapters.Length - 1]
                    |> Seq.sumBy (fun i -> walk (input |> List.skip (i + 1)))
                memo.Add(hd, a) |> ignore
                a
    
printfn "Puzzle 2 : %i" (walk sorted)


// (5289227976704L * 2L) |> Dump
//9256148959232