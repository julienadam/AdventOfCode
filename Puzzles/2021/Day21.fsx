#load "../../Tools.fs"

open System
open System.Collections.Generic
open System.Diagnostics
open System.IO
open AdventOfCode

module Part1 =
    type DeterministicDice = {
        Value: int
        Rolls: int64
    } with
        member this.Roll() =
            let nextVal = if this.Value = 100 then 1 else this.Value + 1
            { this with Value = nextVal ; Rolls = this.Rolls + 1L }
        static member Initial = { Value = 0; Rolls = 0L }

    type PlayerState = {
        Name: string
        Score: int64
        Position: int
    } with 
        member this.Play (dice: DeterministicDice) =
            let d1 = dice.Roll()
            let d2 = d1.Roll()
            let d3 = d2.Roll()
            let absPos = (this.Position + d1.Value + d2.Value + d3.Value) % 10
            let nextPosition = if absPos = 0 then 10 else absPos
            let nextScore = this.Score + (nextPosition |> int64)
            printfn "%s rolled [%i %i %i], moves from %i to %i, total score %i" this.Name d1.Value d2.Value d3.Value this.Position nextPosition nextScore
            { this with Position = nextPosition; Score = nextScore }, d3
        static member CreateInitial position name = { Score = 0; Position = position; Name = name }

    type GameState =
    | GameWon of GameWonState
    | Ongoing of OngoingGameState
    and GameWonState = {
        Dice: DeterministicDice
        Winner: PlayerState
        Loser: PlayerState
    }
    and OngoingGameState = {
        Player1: PlayerState
        Player2: PlayerState
        Dice: DeterministicDice
    } with
        member this.PlayerWins (player:PlayerState) =
            player.Score >= 1000
        member this.PlayTurn() =
            let p1, d1 = this.Player1.Play (this.Dice)
            //printfn "Player 1, moved to %i, total score %i" p1.Position p1.Score
            if this.PlayerWins(p1) then
                GameWon { Dice = d1; Winner = p1; Loser = this.Player2 }
            else
                let p2, d2 = this.Player2.Play d1
                //printfn "Player 2, moved to %i, total score %i" p2.Position p2.Score
                if this.PlayerWins(p2) then
                    GameWon { Dice = d2; Winner = p2; Loser = p1 }
                else
                    Ongoing { this with Player1 = p1; Player2 = p2; Dice = d2 }
        static member Start startPosP1 startPosP2 =
            Ongoing { 
                Player1 = PlayerState.CreateInitial startPosP1 "Player1"; 
                Player2 = PlayerState.CreateInitial startPosP2 "Player2"; 
                Dice = DeterministicDice.Initial 
            }
    
    let solve1 p1Pos p2Pos =
        let rec play state =
            match state with
            | GameWon won -> 
                printfn "Winner is %s with %i, Loser is %s with %i, Rolled %i times" won.Winner.Name won.Winner.Score won.Loser.Name won.Loser.Score won.Dice.Rolls
                won.Loser.Score * won.Dice.Rolls
            | Ongoing ongoing ->
                play (ongoing.PlayTurn())

        play (OngoingGameState.Start p1Pos p2Pos)

//assert(Part1.solve1 4 8 = 739785L)
//Part1.solve1 4 1

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


type Position = int
type Score = int
type Player = Position * Score
type Turn = | Player1 | Player2
type Game = Player * Player * Turn
type Wins = int64 * int64

let inline add2 (a1,a2) (b1,b2) = (a1+b1, a2+b2)

let inline mult2 (a1,a2) x = (a1 * x, a2 * x)

let mutable cacheHits = 0.0
let mutable cacheMiss = 0.0

let rec rollDice (game: Game) roll (memo:Dictionary<Game*int, Wins>)=
    
    let (p1, p1Score), (p2, p2Score), turn = game

    let np1, np2, np1Score, np2Score, nextTurn =
        match turn with
        | Player1 ->
            let nextP1 = p1 + roll
            let nextScoreP1 = p1Score + (nextP1 - 1) % 10 + 1
            (nextP1, p2, nextScoreP1, p2Score, Player2)
        | Player2 ->
            let nextP2 = p2 + roll
            let nextScoreP2 = p2Score + (nextP2 - 1) % 10 + 1
            (p1, nextP2, p1Score, nextScoreP2, Player1)
 
    if (np1Score >= 21) then
        (1L, 0L)
    else if (np2Score >= 21) then
        (0L, 1L)
    else
        match memo.TryGetValue ((game, roll)) with
        | true, v ->
            cacheHits <- cacheHits + 1.0
            v
        | false, _ ->
            cacheMiss <- cacheMiss + 1.0
            let result = 
                diracRollsAndOccurences 
                |> Seq.map (fun (nextRoll, occurs) ->
                    mult2 (rollDice ((np1,np1Score),(np2,np2Score),nextTurn) nextRoll memo) occurs)
                |> Seq.fold add2 (0L, 0L)
            memo.Add((game, roll), result)
            result

let solve2 p1Pos p2Pos =
    let memo = new Dictionary<(Game*int), Wins>()
    diracRollsAndOccurences 
    |> Seq.map (fun (nextRoll, occurs) ->
        mult2 (rollDice ((p1Pos, 0),(p2Pos, 0), Player1) nextRoll memo) occurs)
    |> Seq.fold add2 (0L, 0L)

assert(solve2 4 8 = (444356092776315L, 341960390180808L))
let sw = Stopwatch.StartNew()
let (s1,s2) = solve2 4 1
printfn "Part 2 solution : %i. Took %A. Cache hit %f%%" (max s1 s2) sw.Elapsed ((cacheHits / (cacheMiss + cacheHits)) * 100.0)
