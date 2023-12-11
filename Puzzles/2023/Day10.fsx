
#time
#r "nuget: faqt"
#load "../../Tools.fs"
#load "../../Tools/Directions.fs"
#load "../../Tools/Array2DTools.fs"

open System
open System.IO
open AdventOfCode

type State = 
    {
        row : int; 
        col : int; 
        comingFrom : Direction option;
        steps: (int*int) Set
    }

type Pipes = Direction list

let parseLine row (line:string) =
    line |> Seq.mapi (fun col c ->
        match c with 
        | '|' -> Some [North;South]
        | '-' -> Some [East;West]
        | 'L' -> Some [North;East]
        | 'J' -> Some [North;West]
        | '7' -> Some [South;West]
        | 'F' -> Some [South;East]
        | 'S' -> Some []
        | '.' -> None
        | _ -> failwithf "not a valid pipe %c" c
    )
    |> Seq.toArray

let printPipe = function
    | Some [North;South] -> '┃'
    | Some [East;West]   -> '━' 
    | Some [North;East]  -> '┗'
    | Some [North;West]  -> '┛'
    | Some [South;West]  -> '┓'
    | Some [South;East]  -> '┏' 
    | None               -> '█'
    | Some []            -> 'S'

let connectsSouth r c (grid:Pipes option array2d) = 
    if r > (grid |> Array2D.length1) - 2 then
        false
    else
        match grid[r+1, c] with
        | Some [North;_] -> true
        | _ -> false

let connectsNorth r c (grid:Pipes option array2d) = 
    if r < 1 then
        false
    else
        match grid[r-1, c] with
        | Some [_;South]
        | Some [South;_] -> true
        | _ -> false

let connectsEast r c (grid:Pipes option array2d) = 
    if c > (grid |> Array2D.length2) - 2 then
        false
    else
        match grid[r, c+1] with
        | Some [West;_]
        | Some [_;West] -> true
        | _ -> false

let connectsWest r c (grid:Pipes option array2d) = 
    if c < 1 then
        false
    else
        match grid[r, c-1] with
        | Some [East;_]
        | Some [_;East] -> true
        | _ -> false

let getInput name : (Pipes option array2d * int * int)= 
    let grid = 
        File.ReadAllLines(getInputPath2023 name)
        |> Array.mapi parseLine
        |> array2D

    let startRow, startCol = 
        grid |> Array2DTools.enumArray2d |> Seq.pick (fun p -> 
            match p with 
            | r,c, Some [] -> Some (r,c)
            | _ -> None
        )
    
    grid, startRow, startCol

let getNext (grid:Pipes option array2d) s =
    match grid[s.row,s.col], s.comingFrom with
    | Some [East;West], Some East        -> { s with col = s.col-1 }
    | Some [East;West], Some West        -> { s with col = s.col+1 }
    | Some [North;South], Some North     -> { s with row = s.row+1 }
    | Some [North;South], Some South     -> { s with row = s.row-1 }
    | Some [North;East], Some North      -> { s with col = s.col+1; comingFrom = Some West }
    | Some [North;East], Some East       -> { s with row = s.row-1; comingFrom = Some South }
    | Some [North;West], Some North      -> { s with col = s.col-1; comingFrom = Some East }
    | Some [North;West], Some West       -> { s with row = s.row-1; comingFrom = Some South }
    | Some [South;East], Some South      -> { s with col = s.col+1; comingFrom = Some West }
    | Some [South;East], Some East       -> { s with row = s.row+1; comingFrom = Some North }
    | Some [South;West], Some South      -> { s with col = s.col-1; comingFrom = Some East }
    | Some [South;West], Some West       -> { s with row = s.row+1; comingFrom = Some North }
    | Some p, Some d-> failwithf "Impossible to be moving %A on a %A pipe " d p
    | None, _ -> failwithf "Impossible to not be on a pipe"
    | Some p, None -> failwithf "Cannot have no direction on pipe %A" p

let rec crawl grid (sideA : State) (sideB: State) =
    if (sideA.row, sideA.col) = (sideB.row, sideB.col) then
        sideA.steps, sideB.steps
    else
        let nextA = getNext grid sideA
        let nextB = getNext grid sideB
        let nextAFilled = { nextA with steps = nextA.steps |> Set.add (nextA.row, nextA.col)}
        let nextBFilled = { nextB with steps = nextB.steps |> Set.add (nextB.row, nextB.col)}

        crawl grid nextAFilled nextBFilled

let solve1 input =
    let grid, startRow, startCol = getInput input
    let s = { row = startRow; col = startCol; comingFrom = None; steps = Set.empty}
    let options = [|
        if connectsNorth s.row s.col grid then
            yield { s with row = s.row-1; comingFrom = Some South }
        if connectsEast s.row s.col grid then
            yield { s with col = s.col+1; comingFrom = Some West }
        if connectsSouth s.row s.col grid then
            yield { s with row = s.row+1; comingFrom = Some North }
        if connectsWest s.row s.col grid then
            yield { s with col = s.col-1; comingFrom = Some East }
    |]

    if options.Length <> 2 then
        failwithf "Should not have more than 2 directions available at starting point"
    let sideA = { options[0] with steps = [options[0].row, options[0].col] |> Set.ofSeq }
    let sideB= { options[1] with steps = [options[1].row, options[1].col] |> Set.ofSeq }

    let stepsA, stepsB = crawl grid sideA sideB
    min stepsA.Count stepsB.Count

open Faqt
(solve1 "Day10_sample1.txt").Should().Be(4)
(solve1 "Day10_sample2.txt").Should().Be(8)
solve1 "Day10.txt"

#r "nuget: Pastel"
open Pastel
open System.Collections.Generic
let colorPrint (grid:(Pipes option * ConsoleColor) array2d) =
    for i in [0..grid.GetLength(0) - 1] do
        for j in [0..grid.GetLength(1) - 1] do
            let c, color = grid.[i,j]
            let printed = sprintf "%c" (printPipe c)
            printf "%s" (printed.Pastel(color))
        printfn ""

let findCellsInside (grid:Pipes option array2d) (loopCells:(int*int) Set) =
    let mutable insideCells = new HashSet<int*int>();
    for r = 0 to (grid |> Array2D.length1) - 1 do
        let mutable enteredDir = None
        let mutable walls = 0
        for c = 0 to (grid |> Array2D.length2) - 1 do
            // Replace non-loop cells with ground
            let cell = 
                if loopCells.Contains(r,c) then
                    grid[r,c]
                else
                    None

            // Count number of walls met from left to right on this row
            // Special case if we meet a corner
            // We note the North/South direction of the corner
            // Next corner we meet, if the direction is the same we can ignore it
            // Otherwise we will count it as a wall we traversed
            match cell, enteredDir with
            | Some [North;South], None -> 
                walls <- walls + 1
            | Some [East;West], _ -> 
                ()
            | Some [s;East], None -> 
                enteredDir <- Some s
            | Some [s;West], Some d when d = s -> 
                enteredDir <- None
            | Some [s;West], Some d when d <> s -> 
                walls <- walls + 1
                enteredDir <- None
            | None, None when walls%2=1 -> // Inside
                // Ground cell inside the loop because walls is odd
                insideCells.Add((r,c)) |> ignore
            | None, None when walls%2=0 -> // Not inside
                // Ground cell outside the loop because walls is even
                ()
            | s, d -> 
                failwithf "Invalid state in walk at (%i,%i). %A %A" r c s d

    insideCells

let solve2 input =

    let grid, startRow, startCol = getInput input
    let s = { row = startRow; col = startCol; comingFrom = None; steps = Set.empty}
    let options = [|
        if connectsNorth s.row s.col grid then
            yield North, { s with row = s.row-1; comingFrom = Some South }
        if connectsSouth s.row s.col grid then
            yield South, { s with row = s.row+1; comingFrom = Some North }
        if connectsEast s.row s.col grid then
            yield East, { s with col = s.col+1; comingFrom = Some West }
        if connectsWest s.row s.col grid then
            yield West, { s with col = s.col-1; comingFrom = Some East }
    |]

    let (da, sa) = options[0];
    let (db,sb) = options[1];
    let startPipe = [da;db]


    if options.Length <> 2 then
        failwithf "Should not have more than 2 directions available at starting point"
    let sideA = { sa with steps = [sa.row, sa.col] |> Set.ofSeq }
    let sideB= { sb with steps = [sb.row, sb.col] |> Set.ofSeq }

    let stepsA, stepsB = crawl grid sideA sideB
    let loopCells = (Set.unionMany [stepsA;stepsB;[(startRow, startCol)] |> Set.ofList])

    // Override start pipe
    Array2D.set grid startRow startCol (Some startPipe)

    grid |> Array2DTools.printGridCustom printPipe |> ignore

    let inside = findCellsInside grid loopCells

    let colorized = 
        grid 
        |> Array2D.mapi (fun row col pipe -> 
            if inside.Contains(row,col) then
                None, ConsoleColor.Magenta
            else if stepsA.Contains((row, col)) then 
                pipe, ConsoleColor.Red 
            else if stepsB.Contains((row, col)) then 
                pipe, ConsoleColor.Green 
            else if row = startRow && col = startCol then
                Some(startPipe), ConsoleColor.Blue
            else
                None, ConsoleColor.Black
        ) 
    colorPrint colorized

    inside.Count


(solve2 "Day10_part2_sample1.txt").Should().Be(4)
(solve2 "Day10_part2_sample2.txt").Should().Be(4)
(solve2 "Day10_part2_sample3.txt").Should().Be(8)
(solve2 "Day10_part2_sample4.txt").Should().Be(10)
solve2 "Day10.txt"
