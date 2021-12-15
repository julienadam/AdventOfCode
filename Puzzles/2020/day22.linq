<Query Kind="FSharpProgram" />

let inline splitLines (s:string) = s.Split("\r\n")
let inline splitByEmptyLines (s:string) = s.Split("\r\n\r\n")
let inline tuple2 (array:'a[]) = array.[0], array.[1]
let inline parseIntList (l:string) = l.Split(' ') |> Array.map int |> Array.toList
let toString : char seq -> string = Seq.map string >> String.concat ""
  
let getInputPath file = Path.Combine(Environment.GetFolderPath(Environment.SpecialFolder.UserProfile), @"CloudStation\Data\AdventOfCode\", file)
let path = getInputPath "day22.txt"

let p1Str, p2Str = 
    File.ReadAllText path
    |> splitByEmptyLines
    |> tuple2

let getDeck decklist = decklist |> splitLines |> Array.skip 1 |> Array.map int |> Array.toList

let p1 = p1Str |> getDeck
let p2 = p2Str |> getDeck

let score deck =
    deck 
    |> List.rev 
    |> List.mapi (fun i card -> (i + 1) * card) 
    |> List.sum 

module Puzzle1 =
    
    let rec play deck1 deck2 =
        match deck1, deck2 with
        | [], [] -> failwithf "Both decks are empty, not possible"
        | [], _ -> deck2
        | _, [] -> deck1
        | hd1::tl1, hd2::tl2 when hd1 > hd2 ->
            // Player 1 wins the round
            // Put the winning card and the losing card at the bottom of his deck
            play (tl1 @ [hd1; hd2]) tl2
        | hd1::tl1, hd2::tl2 when hd1 < hd2 ->
            // Player 2 wins the round
            // Put the winning card and the losing card at the bottom of his deck
            play tl1 (tl2 @ [hd2; hd1])
           
    let solution () =
        let winnersDeck = play p1 p2 |> Dump
        score winnersDeck |> Dump
        
module Puzzle2 =

    type Player =
    | Player1
    | Player2
    
    type Win =
    | GameWin of Player * int list
    | InstantPlayer1Win of int list
    
    let solution () =
    
        let rec play (deck1:int list) (deck2:int list) rounds =
            if rounds |> List.exists (fun (d1, d2) -> d1 = deck1 && d2 = deck2) then
                printfn "Instant win of player 1"
                InstantPlayer1Win deck1
            else
                match deck1, deck2 with
                | [], _ -> GameWin (Player2, deck2)
                | _, [] -> GameWin (Player1, deck1)
                | hd1::tl1, hd2::tl2 -> 
                    let nextRounds = (deck1, deck2) :: rounds
                    if tl1.Length >= hd1 && tl2.Length >= hd2 then
                        // Restart a whole game with sub decks
                        let next1 = tl1 |> List.take hd1
                        let next2 = tl2 |> List.take hd2
                        
                        match play next1 next2 [] with
                        | InstantPlayer1Win d -> 
                            printfn "Player 1 won the sub game via instant win"
                            play (tl1 @ [hd1; hd2]) tl2 nextRounds
                        | GameWin (Player1, _) ->
                            printfn "Player 1 won the sub game"
                            play (tl1 @ [hd1; hd2]) tl2 nextRounds
                        | GameWin (Player2, _) ->
                            printfn "Player 2 won the sub game"
                            play tl1 (tl2 @ [hd2; hd1]) nextRounds
                    else
                        if hd2 > hd1 then
                            play tl1 (tl2 @ [hd2; hd1]) nextRounds
                        else
                            play (tl1 @ [hd1; hd2]) tl2 nextRounds
                    // play deck1 deck2 rounds 
                
        match play p1 p2 [] with
        | InstantPlayer1Win deck -> score deck |> Dump
        | GameWin (Player1, deck) -> score deck |> Dump
        | GameWin (Player2, deck) -> score deck |> Dump
        
Puzzle2.solution() |> Dump