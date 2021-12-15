#load "../../Tools.fsx"
open System
open System.IO
open Tools

let path = getInputPath "day04.txt"
//let path = getInputPath "day04_sample1.txt"

let fileLines = File.ReadAllLines(path)

let called = (fileLines |> Seq.head).Split(",") |> Array.map int |> List.ofArray

type BingoCell =
| NotCalled of int
| Called of int

let initialScoreCards = 
    fileLines 
    |> Array.skip 1 
    |> Array.chunkBySize 6 
    |> Array.map (Array.skip 1) 
    |> Array.map (fun lines -> lines |> Array.map (fun line -> line.Trim().Replace("  ", " ").Split(" ") |> Array.map (int >> NotCalled)))
    |> Array.map array2D

let find2D needle (arr: 'a [,]) = Seq.tryPick id <| seq {
    for i in 0..(arr.GetLength 0 - 1) do
        for j in 0..(arr.GetLength 1 - 1) do
            if arr.[i,j] = needle 
                then yield Some (i,j) 
                else yield None
}

let flatten (arr: 'a [,]) = seq {
    for i in 0..(arr.GetLength 0 - 1) do
        for j in 0..(arr.GetLength 1 - 1) do
            yield arr.[i,j]
}

let printCard (scoreCard: BingoCell[,]) =
    for i in 0..(scoreCard.GetLength 0 - 1) do
        for j in 0..(scoreCard.GetLength 1 - 1) do
            match scoreCard.[i,j] with
            | Called i -> printf " (%2i)" i
            | NotCalled i -> printf "  %2i " i
        printfn ""

let calculateScore (scoreCard: BingoCell[,]) lastNumberCalled =
    let sum = scoreCard |> flatten |> Seq.choose (fun c -> match c with | Called _ -> None | NotCalled x -> Some x) |> Seq.sum
    sum * lastNumberCalled

let isWinningRowOrColumn row = Array.TrueForAll(row, (fun x -> match x with | Called _ -> true | _ -> false))

let rec CallNumberOnScoreCard numberCalled (scoreCard:BingoCell[,])  =
    match find2D (NotCalled numberCalled) scoreCard with
    | Some (i, j) -> 
        Array2D.set scoreCard i j (Called numberCalled)
        if isWinningRowOrColumn (scoreCard.[i, *]) then
            Some scoreCard
        else
            if isWinningRowOrColumn (scoreCard.[*, j]) then
                Some scoreCard
            else
                CallNumberOnScoreCard numberCalled scoreCard
        
    | None -> None
    
module Part1 =
    
    let rec CallNumber numbersToCall scoreCards =
        match numbersToCall with
        | [] -> 
            failwith "No more numbers to call"
        | a::rest ->
            match scoreCards |> Array.tryPick (fun s -> CallNumberOnScoreCard a s) with
            | Some s ->
                s, calculateScore s a
            | None ->
                CallNumber rest scoreCards
        
    let Solve() = 
        let winningBoard, score = CallNumber called initialScoreCards
        winningBoard |> printCard
        score |> Dump
        
Part1.Solve()

module Part2 =
    let rec CallNumber numbersToCall scoreCards =
        match numbersToCall, scoreCards with
        | [], _ -> 
            failwith "No more numbers to call"
        | a::rest, [|s|] ->
            match CallNumberOnScoreCard a s with
            | Some w -> 
                w, calculateScore w a
            | None -> CallNumber rest scoreCards
        | a::rest, _ ->
            // Remove winning boards
            let remainingBoards = scoreCards |> Array.choose (fun s -> match CallNumberOnScoreCard a s with | Some _ -> None | None -> Some s)
            CallNumber rest remainingBoards
        
    let Solve() = 
        let winningBoard, score = CallNumber called initialScoreCards
        winningBoard |> printCard
        score |> Dump
        
Part2.Solve()