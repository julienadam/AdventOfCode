<Query Kind="FSharpProgram" />

let getInputPath file = Path.Combine(Environment.GetFolderPath(Environment.SpecialFolder.UserProfile), @"CloudStation\Data\AdventOfCode\2016", file)
let path = getInputPath "day2.txt"

type Direction = | Up | Down | Left | Right

let parseDir c =
    match c with
    | 'U' -> Up
    | 'D' -> Down
    | 'L' -> Left
    | 'R' -> Right
    | x -> failwithf "Unknown direction %c" x

let input = 
    File.ReadAllLines path
    |> Array.map (fun l -> l |> Seq.map parseDir |> Seq.toArray)

let keypad = 
    [
        [1;2;3]
        [4;5;6]
        [7;8;9]
    ] |> array2D

// Row, Column

module Puzzle1 = 

    let move (x, y) direction =
        match (x, y, direction) with
        | x, y, Right when x <= 1 -> x + 1, y
        | x, y, Right when x > 1  -> 2, y
        | x, y, Left  when x > 0  -> x - 1, y
        | x, y, Left  when x <= 0 -> 0, y
        | x, y, Down  when y <= 1 -> x, y + 1
        | x, y, Down  when y > 1  -> x, 2
        | x, y, Up    when y > 0  -> x, y - 1
        | x, y, Up    when y <= 0 -> x, 0
    
    let mutable start = (1,1)

    let solution () =
        input
        |> Array.iter (fun dirs -> 
            let x, y = dirs |> Seq.fold move start
            printf "%i" keypad.[y,x]
            start <- (x,y)
            )
        printfn ""

printfn "Puzzle 1"
Puzzle1.solution()


module Puzzle2 =

    let designedByComiteeKeypad =
        [
            [' ';' ';'1';' ';' ']
            [' ';'2';'3';'4';' ']
            ['5';'6';'7';'8';'9']
            [' ';'A';'B';'C';' ']
            [' ';' ';'D';' ';' ']
        ] |> array2D

    let move (x, y) direction =
        let nx, ny = 
            match (x, y, direction) with
            | x, y, Right -> x + 1, y
            | x, y, Left -> x - 1, y
            | x, y, Down -> x, y + 1
            | x, y, Up -> x, y - 1
        match nx, ny with
        | nx, _ when nx < 0 || nx > 4 -> x, y // out of bounds
        | _, ny when ny < 0 || ny > 4 -> x, y // out of bounds
        | nx, ny -> if designedByComiteeKeypad.[ny, nx] = ' ' then x,y else nx, ny
        
    let solution() =
        let mutable start = (0,2)
        
        input
        |> Array.iter (fun dirs -> 
            let x, y = dirs |> Seq.fold move start
            printf "%c" designedByComiteeKeypad.[y,x]
            start <- (x,y)
            )
        printfn ""

    
printfn "Puzzle 2"
Puzzle2.solution()