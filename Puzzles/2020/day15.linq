<Query Kind="FSharpProgram" />

// let input = [1;3;2] // [9;6;0;10;18;2;1]

type Turn = int

type NumberInfo =
    | SpokenFirst of Turn
    | SpokenAgain of Turn * Turn

let applyRules currentTurn lastSpoken (map:Dictionary<int, NumberInfo>) =
    let result = 
        match map.[lastSpoken] with
        | SpokenFirst _ -> 
            //printfn "%i was never spoken before -> 0" lastSpoken
            0
        | SpokenAgain (i, j) -> 
            //printfn "%i was spoken before on %i and %i -> %i" lastSpoken j i (j-i)
            j - i
        
    let updated = 
        match map.TryGetValue result with
        | false, _ -> 
            //printfn "%i : () -> SpokenFirst(%i)" result currentTurn
            let r = SpokenFirst currentTurn 
            map.Add(result, r)
            r
        | true, SpokenFirst i -> 
            //printfn "%i : SpokenFirst(%i) -> SpokenAgain(%i,%i)" result i i currentTurn
            let r = SpokenAgain(i, currentTurn)
            map.[result] <- r
            r
        | true, SpokenAgain (i,j) -> 
            //printfn "%i : SpokenAgain(%i,%i) -> SpokenAgain(%i,%i)" result i j j currentTurn
            let r = SpokenAgain(j, currentTurn)
            map.[result] <- r
            r
    
    result, map


let solveTurns turns (input: int list) =
    
    let start = input |> Seq.mapi (fun i n -> n, SpokenFirst (i + 1)) |> Map.ofSeq |> Dictionary

    [input.Length + 1..turns] 
    |> List.fold (fun (lastSpoken, map:Dictionary<int, NumberInfo>) turnNumber ->
        applyRules turnNumber lastSpoken map
        )
        ((input |> List.last), start)
    |> fst

let verify tests solve =
    tests 
    |> Seq.iter (fun (input, expected) ->
        let solution = solve input
        if solution = expected then
            printfn "Solution for %A is %i as expected" input solution
        else
            printfn "Solution for %A is %i should have been %i" input solution expected)
                
module Puzzle1 =
    let solve = solveTurns 2020

    let solution() =
        let tests = [
            ([0;3;6], 436)
            ([1;3;2], 1)
            ([2;1;3], 10)
            ([1;2;3], 27)
            ([2;3;1], 78)
            ([3;2;1], 438)
            ([3;1;2], 1836)]
        
        verify tests solve
                
        let puzzleInput = [9;6;0;10;18;2;1]
        printfn "Solution for %A is %i" puzzleInput (solve puzzleInput)

Puzzle1.solution()

module Puzzle2 =
    let solve = solveTurns 30000000
    
    let solution() =
        let tests = [
            ([0;3;6], 175594)
            ([1;3;2], 2578)
            ([2;1;3], 3544142)
            ([1;2;3], 261214)
            ([2;3;1], 6895259)
            ([3;2;1], 18)
            ([3;1;2], 362)]
        
        verify tests solve
                
        let puzzleInput = [9;6;0;10;18;2;1]
        printfn "Solution for %A is %i" puzzleInput (solve puzzleInput)


Puzzle2.solution()