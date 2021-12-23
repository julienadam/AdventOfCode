#load "../../Tools.fsx"

open System
open System.Collections.Generic
open System.Diagnostics
open System.IO
open Tools

//Player 1 starting position: 4
//Player 2 starting position: 1

//type DeterministicDice = {
//    Value: int
//    Rolls: int64
//} with
//    member this.Roll() =
//        let nextVal = if this.Value = 100 then 1 else this.Value + 1
//        { this with Value = nextVal ; Rolls = this.Rolls + 1L }
//    static member Initial = { Value = 0; Rolls = 0L }

//type PlayerState = {
//    Name: string
//    Score: int64
//    Position: int
//} with 
//    member this.Play (dice: DeterministicDice) =
//        let d1 = dice.Roll()
//        let d2 = d1.Roll()
//        let d3 = d2.Roll()
//        let absPos = (this.Position + d1.Value + d2.Value + d3.Value) % 10
//        let nextPosition = if absPos = 0 then 10 else absPos
//        let nextScore = this.Score + (nextPosition |> int64)
//        printfn "%s rolled [%i %i %i], moves from %i to %i, total score %i" this.Name d1.Value d2.Value d3.Value this.Position nextPosition nextScore
//        { this with Position = nextPosition; Score = nextScore }, d3
//    static member CreateInitial position name = { Score = 0; Position = position; Name = name }

//type GameState =
//| GameWon of GameWonState
//| Ongoing of OngoingGameState
//and GameWonState = {
//    Dice: DeterministicDice
//    Winner: PlayerState
//    Loser: PlayerState
//}
//and OngoingGameState = {
//    Player1: PlayerState
//    Player2: PlayerState
//    Dice: DeterministicDice
//} with
//    member this.PlayerWins (player:PlayerState) =
//        player.Score >= 1000
//    member this.PlayTurn() =
//        let p1, d1 = this.Player1.Play (this.Dice)
//        //printfn "Player 1, moved to %i, total score %i" p1.Position p1.Score
//        if this.PlayerWins(p1) then
//            GameWon { Dice = d1; Winner = p1; Loser = this.Player2 }
//        else
//            let p2, d2 = this.Player2.Play d1
//            //printfn "Player 2, moved to %i, total score %i" p2.Position p2.Score
//            if this.PlayerWins(p2) then
//                GameWon { Dice = d2; Winner = p2; Loser = p1 }
//            else
//                Ongoing { this with Player1 = p1; Player2 = p2; Dice = d2 }
//    static member Start startPosP1 startPosP2 =
//        Ongoing { 
//            Player1 = PlayerState.CreateInitial startPosP1 "Player1"; 
//            Player2 = PlayerState.CreateInitial startPosP2 "Player2"; 
//            Dice = DeterministicDice.Initial 
//        }
    
//let solve1 p1Pos p2Pos =
//    let rec play state =
//        match state with
//        | GameWon won -> 
//            printfn "Winner is %s with %i, Loser is %s with %i, Rolled %i times" won.Winner.Name won.Winner.Score won.Loser.Name won.Loser.Score won.Dice.Rolls
//            won.Loser.Score * won.Dice.Rolls
//        | Ongoing ongoing ->
//            play (ongoing.PlayTurn())

//    play (OngoingGameState.Start p1Pos p2Pos)

//assert(solve1 4 8 = 739785L)
//solve1 4 1

let diracRollsAndOccurences = 
    let enumDiracRolls () = seq {
        for i = 1 to 3 do
            for j = 1 to 3 do
                for k = 1 to 3 do
                    yield [i;j;k]
    }

    enumDiracRolls () 
    |> Seq.map List.sum 
    |> Seq.groupBy id 
    |> Seq.map (fun (g,v) -> g, v |> Seq.length |> int64) 
    |> Seq.toList

type Player = int * int
type Turn = | Player1 | Player2
type Game = Player * Player * Turn
type Wins = int64 * int64

let validateInvariants game =
    let (p1s, p1p), (p2s, p2p), _ = game;
    assert(p1p > 0)
    assert(p1p <= 10)
    assert(p2p > 0)
    assert(p2p <= 10)
    assert(p1s >= 0)
    assert(p2s >= 0)

let iprintfn i format = 
    for i = 1 to i do printf "\t"
    printfn format

let applyRoll ((pScore, pPos):Player) roll =
    (pScore + roll, 1 + (pPos + roll - 1) % 10)

let rec compute (g:Game) (memo: Dictionary<Game, Wins>) depth =
    iprintfn depth "%A. Memo %i" g memo.Count
    validateInvariants g
    match g with
    | (p1s, _), (_, _), Player1 when p1s >= 21 ->
        failwithf "Player 1 has won during player 2's turn"
    | (p1s, _), (_, _), Player2 when p1s >= 21 ->
        memo.[g] <- (1L, 0L)
        iprintfn depth  "Player1 wins"
        (1L, 0L)
    | (_, _), (p2s, _), Player2 when p2s >= 21 ->
        failwithf "Player 2 has won during player 1's turn"
    | (_, _), (p2s, _), _ when p2s >= 21 ->
        iprintfn depth  "Player2 wins"
        memo.[g] <- (0L, 1L)
        (0L, 1L)
    | _ ->
        match memo.TryGetValue g with
        | true, v -> 
            iprintfn depth  "Returning memoized value %A" v
            v
        | false, _ ->
            let p1, p2, turn = g
            let computeNext roll =
                match turn with
                | Player1 -> compute ((applyRoll p1 roll), p2, Player2) memo (depth + 1)
                | Player2 -> compute (p1, (applyRoll p2 roll), Player1) memo (depth + 1)
            
            let games = 
                diracRollsAndOccurences
                |> List.map (fun (roll, occurs) -> roll, occurs, (computeNext roll))
            iprintfn depth "%A" games
            let result = 
                games |> List.fold (
                    fun (totalWins1, totalWins2) (_, occurs, (w1, w2)) ->
                        totalWins1 + w1 * occurs, totalWins2 + w2 * occurs) (0L,0L)
            iprintfn depth  "Computed sub games %A" result
            memo.[g] <- result
            result

let solve2 p1Pos p2Pos =
    compute ((0, p1Pos), (0, p2Pos), Player1) (new Dictionary<Game, Wins>()) 0
// 
solve2 4 8 |> Dump

let memo = new Dictionary<Game, Wins>()
let result = compute ((17, 4), (18, 8), Player2) (memo) 0
memo |> Dump

//(100571635749L, 43494262306L)
//444356092776315, 341960390180808