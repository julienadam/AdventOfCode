
#time
#load "../../Tools.fs"

open System
open System.IO
open AdventOfCode
open Array2DTools

let getInput p = 
    File.ReadAllLines(getInputPath2023 p)
    |> Array.map (fun s -> s.ToCharArray())
    // |> array2D

let isSymbol c =
    match c with
    | d when Char.IsAsciiDigit(d) -> false
    | '.' -> false
    | _ -> true

let charsToInt chars = String(chars |> Seq.toArray) |> int

let findParts grid row chars =
    let rec findPartsRec (grid: char array array) (row:int) (col: int) (chars:char list) isPartNo (acc : char list) res =
        let hasAdjacentSymbol () =
            getAdjacentCoordsDiags row col (grid.Length) grid[0].Length  
            |> Seq.exists (fun (r,c) -> grid.[r].[c] |> isSymbol)

        // (isPartNo, chars, acc, res) |> Dump

        match chars with 
        | c :: rest when Char.IsDigit(c) ->
            match isPartNo || hasAdjacentSymbol (), rest with
            | true, [] -> // Last character is a digit in a part number
                let part_number = (List.append acc [c]) |> charsToInt
                part_number :: res
            | true, _ -> // non last character is a digit in a part number accumulate and continue
                findPartsRec grid row (col + 1) rest true (List.append acc [c]) res
            | false, [] -> // Not a part number, end of the line, return
                res
            | false, _ -> // Not a part number but characters left, accumulate and continue
                findPartsRec grid row (col + 1) rest false (List.append acc [c]) res
        | _ :: rest -> // Not a digit
            match isPartNo, acc with
            | true, _::_ -> // Valid part number in accumulator, add it to results and continue
                let part_number = acc |> charsToInt
                findPartsRec grid row (col + 1) rest false [] (part_number :: res)
            | _, _ -> // Nothing valid in accumulator, continue
                findPartsRec grid row (col + 1) rest false [] res
        | _ -> res

    findPartsRec grid row 0 chars false [] []

let solve1 input =
    let grid = getInput input
    
    grid
    |> Array.mapi (fun line chars ->
        printfn "Line %i" line
        findParts grid line (chars |> Array.toList)
    )
    |> Seq.collect id
    |> Seq.sum

let findNumberAtPos (grid: char array array) row col =
    let chars = grid[row]

    // Look left
    let rec findLeft col acc =
        match col with
        | -1 -> (col + 1), acc
        | _ ->
            match chars[col] with
            | c when Char.IsDigit(c) ->
                findLeft (col - 1) (c :: acc)
            | _ -> (col + 1), acc

    // Look right
    let rec findRight col acc =
        match col with
        | c when c = (chars |> Array.length) -> (col - 1), acc
        | _ ->
            match chars.[col] with
            | c when Char.IsDigit(c) ->
                findRight (col + 1) (List.append acc [c])
            | _ -> (col - 1), acc

    
    match chars[col] with
    | c when Char.IsDigit(c) ->
        // Combine current char with left and right
        // And return the position of the leftmost char
        let (left, lc) = findLeft (col-1) []
        let (right, rc) = findRight (col+1) []
        let number = List.concat [lc; [c]; rc] |> charsToInt
        let leftMost = 
            match lc with
            | [] -> col
            | _ -> left
        Some(leftMost, number)
    | _ -> 
        None

let findAdjacentNumbers (grid: char array array) row col =
    getAdjacentCoordsDiags row col grid.Length grid[0].Length
    |> Seq.choose(fun (row, col) -> findNumberAtPos grid row col)
    |> Seq.distinct // Remove duplicates (i.e. same number at the same position)
    |> Seq.map snd
    
let solve2 input =
    let grid = getInput input
    let gearPositions = 
        grid
        |> array2D
        |> enumArray2d 
        |> Seq.choose(fun (row, col, c) ->
            match c with
            | '*' -> (row, col) |> Some
            | _ -> None
        )
        
    gearPositions 
    |> Seq.choose (fun (row, col) -> 
        // Find all adjacent numbers
        match (findAdjacentNumbers grid row col) |> Seq.toList with
        | [a;b] -> // Exactly 2 numbers, return their product
            Some(a * b)
        | _ -> None
    )
    |> Seq.sum

solve1 "Day03.txt"
solve2 "Day03.txt"