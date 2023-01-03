#time
#load "../../Tools.fs"

open System
open System.IO
open Checked
open AdventOfCode

let parseMap (input:string array) =
    let width = input.[0].Length
    let height = input.Length

    let blizzards = 
        seq {
            for row, line in (input |> Seq.indexed) do
                for col, c in line |> Seq.indexed |> Seq.filter (fun (_,c) -> not (c = ' ')) do
                    match c with
                    | '>' -> yield row, col, East
                    | '^' -> yield row, col, North
                    | '<' -> yield row, col, West
                    | 'v' -> yield row, col, South
                    | _ -> ()
        } |> Seq.toList
    blizzards, height, width

let getInput p =
    File.ReadAllLines(getInputPath2022 p)
    |> parseMap

let getBlizzPositions height width minute blizzards =
    let w' = width - 2
    let h' = height - 2

    blizzards |> List.map (fun (r, c, b) ->
        match b with
        | North -> 1 + (h' * minute + r - 1 - minute) % h', c, North
        | South -> (r - 1 + minute) % h' + 1, c, South
        | East -> r, (c - 1 + minute) % w' + 1, East
        | West -> r, 1 + (w' * minute + c - 1 - minute) % w', West
    )

let print (blizzards, height, width) elves =
    for r = 0 to height - 1 do
        for c = 0 to width - 1 do
            if r = 0 && not(c = 1) then
                printf "#" 
            else if (r = height - 1) && not(c = width - 2) then
                printf "#" 
            else if c = 0 || (c = width - 1) then
                printf "#"
            else
                if elves |> Set.contains (r,c) then
                    printf "E"
                else
                    match blizzards |> List.filter (fun (br,bc,dir) -> r = br && c = bc) |> List.map (fun (_,_,d) -> d) with
                    | [] -> printf "."
                    | [North] -> printf "^"
                    | [South] -> printf "v"
                    | [East] -> printf ">"
                    | [West] -> printf "<"
                    | [_;_] -> printf "2"
                    | [_;_;_] -> printf "3"
                    | [_;_;_;_] -> printf "4"
                    | _ -> failwithf "not possible ?"
        printfn ""
    (blizzards, height, width)

let printAfter (blizzards, height, width) minute =
    let finalB = getBlizzPositions height width minute blizzards
    print (finalB, height, width)

 //Let's try something else
 //At each turn put a elf everywhere possible (i.e around an existing elf and not in a blizzard)
 //The turn we put an elf at the exit, it's a win

let solve (blizzards, height, width) entrance exit startMin = 
    //let entrance = 0, 1
    //let exit = height - 1, width - 2

    let rec multiplyElves minute (elves:Set<int*int>) =
        let blizzPositions = blizzards |> getBlizzPositions height width minute
        let blizzards = 
            blizzPositions
            |> List.map (fun (r,c,_) -> (r,c))
            |> Set.ofList
        let survivingElves = Set.difference elves blizzards

        if survivingElves |> Set.contains exit then 
            minute
        else
            let nextGeneration = 
                survivingElves |> Seq.collect (fun (r,c) -> seq {
                    // Northern neighbor
                    if r > 1 || (r = 1 && c = 1) then
                        yield ((r - 1), c)
                    // Southern neighbor
                    if r < height - 2 || (r = height - 2 && c = width - 2) then
                        yield (r + 1), c
                    // Western neighbor
                    if c > 1 && r > 0 && r < height - 1 then
                        yield r, c - 1
                    // Eastern neighbor
                    if c < width - 2 && r > 0 && r < height - 1 then
                        yield r, c + 1
                    // Wait here
                    yield r,c
                }) |> Set.ofSeq

            multiplyElves (minute + 1) nextGeneration

    multiplyElves startMin ([entrance] |> Set.ofList)

let solve1 (blizzards, height, width) =
    let entrance = 0, 1
    let exit = height - 1, width - 2
    solve (blizzards, height, width) entrance exit 0

getInput "Day24.txt"
|> solve1

let solve2 (blizzards, height, width) =
    let entrance = 0, 1
    let exit = height - 1, width - 2
    let trip1 = solve (blizzards, height, width) entrance exit 0
    let trip2 = solve (blizzards, height, width) exit entrance trip1
    let trip3 = solve (blizzards, height, width) entrance exit trip2
    trip3

getInput "Day24.txt"
|> solve2
    