#time
#load "../../Tools.fs"
#load "../../Tools/MathEx.fs"
#r "nuget: faqt"


open System.IO
open AdventOfCode

type Turn = | Left | Right

let charToTurn = function | 'R' -> Turn.Right | 'L' -> Turn.Left | _ -> failwith "not a valid turn"

let inline parseNode (line:string) = line.Substring(0, 3), (line.Substring(7,3), line.Substring(12,3))

let getInput name = 
    let lines = File.ReadAllLines(getInputPath2023 name)
    let turns = lines[0] |> Seq.map charToTurn |> Array.ofSeq
    let nodes = lines |> Seq.skip 2 |> Seq.map parseNode |> Map.ofSeq
    turns, nodes

let solve1 input =
    let moves, nodes  = getInput input

    let rec move turn (node:string*(string*string)) =
        match node with
        | "ZZZ", (_,_) -> turn
        | s, _ ->
            // printfn "Turn %i Node %s" turn s
            match moves[turn % moves.Length], node with
            | Left, (_, (l,_)) -> move (turn + 1) (l, nodes[l])
            | Right, (_,(_,r)) -> move (turn + 1) (r, nodes[r])

    move 0 ("AAA", nodes["AAA"])

open Faqt

(solve1 "Day08_sample.txt").Should().Be(2)
(solve1 "Day08_sample2.txt").Should().Be(6)
solve1 "Day08.txt"

let solve2 input =
    let moves, nodes  = getInput input

    // Number of turns for a single node to find its destination
    let rec findPeriod turn (node:string*(string*string)) =
        match node with
        | s, (_,_) when s.EndsWith('Z') -> turn
        | s, _ ->
            let t = (turn % (moves.Length |> int64)) |> int
            match moves[t], node with
            | Left, (_, (l,_)) -> findPeriod (turn + 1L) (l, nodes[l])
            | Right, (_,(_,r)) -> findPeriod (turn + 1L) (r, nodes[r])

    let initialNodes =
        nodes 
        |> Seq.choose(fun kvp ->  match kvp.Key.EndsWith('A') with | true -> (kvp.Key, kvp.Value) |> Some | _ -> None) 
        |> List.ofSeq

    // Calculate the period for each
    // Then find the LCM of the periods, which will give us the solution
    initialNodes 
    |> List.map (findPeriod 0L)
    |> List.fold MathEx.lcm64 1L

(solve2 "Day08_sample3.txt").Should().Be(6)
solve2 "Day08.txt"
