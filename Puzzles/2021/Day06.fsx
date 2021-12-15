#load "../../Tools.fsx"

open System
open System.IO
open Tools

let path = getInputPath "day06.txt"
//let path = getInputPath "day06_sample1.txt"

let input = (File.ReadLines(path) |> Seq.head).Split(",") |> Array.map int |> Array.toList

module Part1 =
    
    // Naive impl, doesn't scale to part 2s
    let evolve lanternFish =
        match lanternFish with
        | 0 -> 6, true
        | x -> x - 1, false
        
    let printFishes lanternFishes = Console.WriteLine(String.Join(',', lanternFishes |> List.map string |> List.toArray));

    let evolveAll lanternFishes = 
        let evolvedFishes = lanternFishes |> List.map evolve
        let newFishes = evolvedFishes |> List.filter snd |> List.map (fun _ -> 8)
        let r = List.concat [evolvedFishes |> List.map fst; newFishes]
        //printFishes r
        r
    
    let Solve() = 
        [1..80] |> Seq.fold (fun fishes _ -> evolveAll fishes) input |> Seq.length |> Dump
        ()

Part1.Solve()

module Part2 =

    let evolveMap (state: Map<int, int64>) = 
        let numNewFishes = match state |> Map.tryFind 0 with | Some count -> count | _ -> 0L
        
        let evolvedState = 
            state 
            |> Map.toList 
            |> List.filter (fun (timer, _) -> timer > 0)
            |> List.map (fun (timer, count) ->
                match timer with
                | 7 -> (6, count + numNewFishes)
                | x -> (x - 1, count))
            
        let result = (8, numNewFishes) :: evolvedState
        result |> Map.ofList
        
    let Solve() = 
        let initialFishMap = 
            input 
            |> List.groupBy id 
            |> List.map (fun (g, members) -> g, members |> List.length) 
            |> Map.ofList
            
        let fullInitialFishMap = 
            [0..8] 
            |> List.map (fun i -> match initialFishMap |> Map.tryFind i with | Some count -> (i, count |> int64) | _ -> (i, 0L))
            |> Map.ofList
            
        [1..256] |> Seq.fold (fun fishMap _ -> evolveMap fishMap) fullInitialFishMap |> Dump
        ()
        
Part2.Solve()
