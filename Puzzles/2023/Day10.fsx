
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

type Pipes = 
    | NorthSouth         // |
    | EastWest           // -
    | NorthEast          // L
    | NorthWest          // J
    | SouthWest          // 7
    | SouthEast          // F
    | Start              // S

let parseLine row (line:string) =
    line |> Seq.mapi (fun col c ->
        match c with 
        | '|' -> Some NorthSouth
        | '-' -> Some EastWest
        | 'L' -> Some NorthEast
        | 'J' -> Some NorthWest
        | '7' -> Some SouthWest
        | 'F' -> Some SouthEast
        | 'S' -> Some Start
        | '.' -> None
        | _ -> failwithf "not a valid pipe %c" c
    )
    |> Seq.toArray

let printPipe = function
    | Some NorthSouth -> '┃'
    | Some EastWest   -> '━' 
    | Some NorthEast  -> '┗'
    | Some NorthWest  -> '┛'
    | Some SouthWest  -> '┓'
    | Some SouthEast  -> '┏' 
    | Some Start      -> '█'
    | None            -> '░'

let connectsSouth r c (grid:Pipes option array2d) = 
    if r > (grid |> Array2D.length1) - 2 then
        false
    else
        match grid[r+1, c] with
        | Some NorthSouth
        | Some NorthEast
        | Some NorthWest -> true
        | _ -> false

let connectsNorth r c (grid:Pipes option array2d) = 
    if r < 1 then
        false
    else
        match grid[r-1, c] with
        | Some NorthSouth
        | Some SouthEast
        | Some SouthWest -> true
        | _ -> false

let connectsEast r c (grid:Pipes option array2d) = 
    if c > (grid |> Array2D.length2) - 2 then
        false
    else
        match grid[r, c+1] with
        | Some EastWest
        | Some NorthWest
        | Some SouthWest -> true
        | _ -> false

let connectsWest r c (grid:Pipes option array2d) = 
    if c < 1 then
        false
    else
        match grid[r, c-1] with
        | Some EastWest
        | Some NorthEast
        | Some SouthEast -> true
        | _ -> false

let getInput name : (Pipes option array2d * int * int)= 
    let grid = 
        File.ReadAllLines(getInputPath2023 name)
        |> Array.mapi parseLine
        |> array2D

    let startRow, startCol = 
        grid |> Array2DTools.enumArray2d |> Seq.pick (fun p -> 
            match p with 
            | r,c, Some Start -> Some (r,c)
            | _ -> None
        )
    
    grid, startRow, startCol

let getNext (grid:Pipes option array2d) s =
    match grid[s.row,s.col], s.comingFrom with
    | Some EastWest, Some East        -> { s with col = s.col-1 }
    | Some EastWest, Some West        -> { s with col = s.col+1 }
    | Some NorthSouth, Some North     -> { s with row = s.row+1 }
    | Some NorthSouth, Some South     -> { s with row = s.row-1 }
    | Some NorthEast, Some North      -> { s with col = s.col+1; comingFrom = Some West }
    | Some NorthEast, Some East       -> { s with row = s.row-1; comingFrom = Some South }
    | Some NorthWest, Some North      -> { s with col = s.col-1; comingFrom = Some East }
    | Some NorthWest, Some West       -> { s with row = s.row-1; comingFrom = Some South }
    | Some SouthEast, Some South      -> { s with col = s.col+1; comingFrom = Some West }
    | Some SouthEast, Some East       -> { s with row = s.row+1; comingFrom = Some North }
    | Some SouthWest, Some South      -> { s with col = s.col-1; comingFrom = Some East }
    | Some SouthWest, Some West       -> { s with row = s.row+1; comingFrom = Some North }
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



let isVertical = function
    | None -> false
    | Some Start -> failwithf "handle start ..."
    | Some EastWest -> false
    | _ -> true
    
let isHorizontal = function
    | None -> false
    | Some Start -> failwithf "handle start ..."
    | Some NorthSouth -> false
    | _ -> true

let isCellInLoop (grid:Pipes option array2d) row col (loopCells: (int*int) Set) =
    if loopCells.Contains(row, col) then 
        false 
    else
        let north = loopCells |> Seq.filter (fun (r,c) -> grid[r,c] |> isHorizontal && r < row && c = col) |> Seq.length
        if north%2 = 0 then 
            false
        else
            let south = loopCells |> Seq.filter (fun (r,c) -> grid[r,c] |> isHorizontal && r > row && c = col) |> Seq.length
            if south%2 = 0 then
                false
            else
                let east = loopCells |> Seq.filter (fun (r,c) -> grid[r,c] |> isVertical && r = row && c > col) |> Seq.length
                if east%2 = 0 then
                    false
                else
                    let west = loopCells |> Seq.filter (fun (r,c) -> grid[r,c] |> isVertical && r = row && c < col) |> Seq.length
                    if west%2 = 0 then
                        false
                    else 
                            true

let solve2 input =
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
    let loopCells = (Set.unionMany [stepsA;stepsB;[(startRow, startCol)] |> Set.ofList])

    grid 
    |> Array2DTools.enumArray2d 
    |> Seq.filter (fun (row, col, _) -> 
        isCellInLoop grid row col loopCells
    )
    |> Seq.length

(solve2 "Day10_part2_sample1.txt").Should().Be(4)
(solve2 "Day10_part2_sample2.txt").Should().Be(4)
(solve2 "Day10_part2_sample3.txt").Should().Be(8)
solve2 "Day10.txt"


// let fill (grid) (loopCells:(int*int) Set) =
//     let mutable total = 0
//     for r = 0 to (grid |> Array2D.length1) - 1 do
//         let intersections = seq {
//             for c = 0 to (grid |> Array2D.length2) - 1 do
//                 if loopCells.Contains(r,c) then 
//                     yield c
//         }

//         let fill = 
//             intersections 
//             |> Seq.pairwise
//             |> Seq.map (fun (a,b) -> if b - a > 2 then b - a else 0)
//             |> Seq.sum
//         printfn "Line %i. Filled %i" r fill
//         total <- total + fill
    
//     total


// let rec crawl2 grid (moving : State) (start: State) visited=
//     if (moving.row, moving.col) = (start.row, start.col) then
//         visited |> Set.add (start.row, start.col)
//     else
//         let nextA = getNext grid moving 
//         let nextVisited = (visited |> Set.add (moving.row, moving.col))
//         crawl2 grid nextA start nextVisited


// let isCellInLoop row col (loopCells: (int*int) Set) =
//     if loopCells.Contains(row, col) then 
//         false 
//     else
//         let north = loopCells |> Seq.filter (fun (r,c) -> r < row && c = col) |> Seq.length
//         if north%2 = 0 then 
//             false
//         else
//             let south = loopCells |> Seq.filter (fun (r,c) -> r > row && c = col) |> Seq.length
//             if south%2 = 0 then
//                 false
//             else
//                 let east = loopCells |> Seq.filter (fun (r,c) -> r = row && c > col) |> Seq.length
//                 if east%2 = 0 then
//                     false
//                 else
//                     let west = loopCells |> Seq.filter (fun (r,c) -> r = row && c < col) |> Seq.length
//                     if west%2 = 0 then
//                         false
//                     else 
         

    // let colorized = 
    //     grid 
    //     |> Array2D.mapi (fun row col pipe -> 
    //         if stepsA.Contains((row, col)) then 
    //             pipe, ConsoleColor.Red 
    //         else if stepsB.Contains((row, col)) then 
    //             pipe, ConsoleColor.Green 
    //         else if pipe = Some(Start) then
    //             Some(Start), ConsoleColor.Blue
    //         else
    //             None, ConsoleColor.Black
    //     ) 
    // colorPrint colorized
    // |> Array2DTools.printGridCustom printPipe 
    // |> ignore

// #r "nuget: Pastel"
// open Pastel
// let colorPrint (grid:(Pipes option * ConsoleColor) array2d) =
//     for i in [0..grid.GetLength(0) - 1] do
//         for j in [0..grid.GetLength(1) - 1] do
//             let c, color = grid.[i,j]
//             let printed = sprintf "%c" (printPipe c)
//             printf "%s" (printed.Pastel(color))
//         printfn ""


// let solve2 input =

//     let grid, startRow, startCol = getInput input
//     let startState = { row = startRow; col = startCol; comingFrom = None; steps = 0}

//     let options = seq {
//         if connectsNorth startState.row startState.col grid then
//             yield { startState with row = startState.row-1; comingFrom = Some South }
//         if connectsEast startState.row startState.col grid then
//             yield { startState with col = startState.col+1; comingFrom = Some West }
//         if connectsSouth startState.row startState.col grid then
//             yield { startState with row = startState.row+1; comingFrom = Some North }
//         if connectsWest startState.row startState.col grid then
//             yield { startState with col = startState.col-1; comingFrom = Some East }
//     }

//     let moving = options |> Seq.head
//     let loopCells = crawl2 grid moving startState Set.empty

//     grid 
//     |> Array2D.mapi (fun row col pipe -> 
//         if loopCells.Contains((row, col)) then pipe else None
//     ) 
//     |> Array2DTools.printGridCustom printPipe 
//     |> ignore

//     grid 
//     |> Array2DTools.enumArray2d 
//     |> Seq.filter (fun (row, col, _) -> 
//         isCellInLoop row col loopCells
//     )
//     |> Seq.length
// let fill (grid) (loopCells:(int*int) Set) =
//     let mutable total = 0
//     for r = 0 to (grid |> Array2D.length1) - 1 do
//         let intersections = seq {
//             for c = 0 to (grid |> Array2D.length2) - 1 do
//                 if loopCells.Contains(r,c) then 
//                     yield c
//         }

//         let fill = 
//             intersections 
//             |> Seq.pairwise
//             |> Seq.map (fun (a,b) -> if b - a > 2 then b - a else 0)
//             |> Seq.sum
//         printfn "Line %i. Filled %i" r fill
//         total <- total + fill
    
//     total
    //fill grid loopCells

    // let outside = fill grid loopCells 
    // printfn "On loop      : %i" loopCells.Count
    // printfn "Outside loop : %i" outside.Count
    // let totalArea = (grid |> Array2D.length1) * (grid |> Array2D.length2)
    // printfn "Total area   : %i" totalArea
    // let inside = (totalArea - loopCells.Count - outside.Count)
    // printfn "Inside loop  : %i" inside
    // inside
    // let origGrid, origRow, origCol = getInput input
    // let grid = Array2D.create ((origGrid |> Array2D.length1) + 2) ((origGrid |> Array2D.length2) + 2) None
    // Array2D.blit origGrid 0 0 grid 1 1 (origGrid |> Array2D.length1) (origGrid |> Array2D.length2)
    // let startState = { row = origRow + 1; col = origCol + 1; comingFrom = None; steps = 0}

// let fill grid (loopCells:(int*int) Set) =
//     let visited = new HashSet<(int*int)>()
//     let rec fillRec (row:int) (col:int) =
//         if loopCells.Contains (row, col) = false then
//             visited.Add((row, col)) |> ignore

//         let candidates = [
//              if row > 0 then yield row - 1, col
//              if col > 0 then yield row, col - 1
//              if row < (grid |> Array2D.length1) - 1 then yield row + 1, col
//              if col < (grid |> Array2D.length2) - 1 then yield row, col + 1
//         ]

//         let validCandidates = 
//             candidates |> List.filter (fun c -> (loopCells.Contains(c) = false) && (visited.Contains(c) = false))
        
//         if validCandidates.IsEmpty then
//             ()

//         validCandidates |> List.iter (fun (r, c) -> fillRec r c)

//     fillRec 0 0
//     visited
