
#time
#load "../../Tools.fs"

open System
open System.IO
open AdventOfCode

let parseLine (line:string) =
    let parseIntList = ssplitNoEmpty " " >> Seq.map trim >> Seq.map int
    let numbers = line |> ssplit2 ": " |> snd
    let l, r = numbers |>  ssplit2 " | "
    l |> parseIntList |> Set.ofSeq, r |> parseIntList |> List.ofSeq

let getInput p = 
    File.ReadAllLines(getInputPath2023 p)
    |> Array.mapi (fun i l -> i + 1, parseLine l)

let countWins (winningNumbers: Set<int>) (myNumbers: int list) =
    myNumbers 
    |> Seq.filter (fun n -> winningNumbers |> Set.contains n) 
    |> Seq.length

let cardScore (winningNumbers: Set<int>) (myNumbers: int list) =
    let wins = countWins winningNumbers myNumbers
    if wins > 0 then pow2 (wins - 1) else 0

let solve1 input = 
    getInput input
    |> Seq.map (fun (_, (winners, numbers)) -> 
        cardScore winners numbers
    )
    |> Seq.toList
    |> Seq.sum

let solve2 input =
    
    let countWinsForEachCard input =
        getInput input
        |> Seq.map (fun (cardNo, (winners, numbers)) -> cardNo, countWins winners numbers)
        |> Seq.toList
    
    let cardWins = countWinsForEachCard input

    let getCardsWon cardNo numWins =
        if cardNo + numWins <= cardWins.Length then
            // Enough cards left
            [(cardNo + 1)..(cardNo + numWins)]
        else if cardNo < input.Length then
            // Some remaining, but not all
            [(cardNo + 1)..(min cardWins.Length (cardNo + numWins))]
        else
            // No cards left
            []

    // Card ids and the list of card ids it will win
    let winsByCard = 
        cardWins 
        |> Seq.map (fun (cardNo, wins) -> cardNo, getCardsWon cardNo wins) 

    // Initial state, one copy of each
    let state = 
        cardWins
        |> Seq.map (fun (cardNo, _) -> cardNo, 1)
        |> Map.ofSeq

    // For each card id that is won, add count number of copies to the state
    let applyWins (count:int) (wins:int list) (cardCounts:Map<int, int>) =
        wins 
        |> Seq.fold (fun (acc:Map<int,int>) winningCardId ->
            let currentCount = acc[winningCardId]
            acc |> Map.add winningCardId (currentCount + count)) cardCounts

    // Scan all card ids and apply the winnings
    let finalCounts = 
        winsByCard 
        |> Seq.fold (fun (acc:Map<int, int>) (id, wins) -> 
            acc 
            |> applyWins acc[id] wins
        ) state

    finalCounts 
    |> Map.toSeq
    |> Seq.map snd
    |> Seq.sum

solve1 "Day04.txt"
solve2 "Day04.txt"

