#time "on"
#load "../../Tools.fs"
#load "../../Tools/Array2DTools.fs"
#r "nuget: NFluent"

open System
open System.IO
open System.Linq
open AdventOfCode
open AdventOfCode.Array2DTools
open Checked
open NFluent

let getInput name =
    let split =
        File.ReadAllText(getInputPath2025 name)
        |> ssplit "\n\n"
    
    let regions =
        split.Last()
        |> ssplit "\n"
        |> Seq.map (fun r -> r |> ssplit2 ": ")
        |> Seq.map (fun (size, quantities) -> size |> ssplit2 "x", quantities |> splitSpaceIntList)
        |> Seq.map (fun ((r,c), quantities) -> (int r, int c), quantities)
        |> Seq.toArray
        
    let pieces =
        split[0..split.Length - 2] 
        |> Seq.map (fun l -> l |> ssplit "\n") 
        |> Seq.map (fun s -> s |> Seq.skip 1 |> array2D |> Array2D.map (fun c -> if c = '#' then 1 else 0))
        |> Seq.toArray
        
    regions, pieces
      
let getAllDistinctRotationsAndMirrors piece =
    seq {
        yield piece
        yield piece |> flipV
        let r1 = piece |> rotate
        yield r1
        yield r1 |> flipV
        let r2 = r1 |> rotate
        yield r2
        yield r2 |> flipV
        let r3 = r2 |> rotate
        yield r3
        yield r3 |> flipV
    } |> Seq.distinct
    
let enumerateValidPlacements  boardRows boardCols piece=
    seq {
        for r = 0 to (boardRows - (piece |> Array2D.length1)) do
            for c = 0 to (boardCols - (piece |> Array2D.length2)) do
                yield (r,c)
    }
    
#r "nuget: DlxLib"

#load "../../Tools/SeqEx.fs"

open DlxLib

let canPlacePiecesOnBoard (pieces:int[,] array) (boardHeight, boardWidth) =
    let printer i = if i = 0 then '.' else '#'
    let inline rcToDlxCol r c rowSize = r * rowSize + c

    // We're going to use an existing implementation of Knuth's Algorithm X Dancing Links (DLX)
    // For that we need a matrix with those columns :
    // - one column for each square in the target grid (e.g. 12*5 would result in 60 columns
    // - one column for each individual piece (if the same piece is present 2 times, add 2 columns etc.) 
    // Each row will represent one variant of each piece (combination of rotations and flip)
    // at one valid placement in the grid
    
    // Compute a sequence of all possible places for all possible rotations / flip
    let placedVariants =
        pieces
        |> Seq.indexed
        |> Seq.collect (fun (id, p) ->
            SeqEx.crossproduct (p |> enumerateValidPlacements boardHeight boardWidth) (p |> getAllDistinctRotationsAndMirrors)
            |> Seq.map (fun p -> id, p))
        // |> Seq.toArray |> Dump

    // Build the DLX matrix
    let matrix =
        placedVariants
        |> Seq.map (fun (id, ((r,c), variant)) ->
            // Initialize an empty row
            let mutable row = Array.zeroCreate<int> (boardHeight*boardWidth+pieces.Length)
            // For each filled square in the piece, mark the corresponding column with a 1 
            for offR = 0 to variant |> maxR do
                for offC = 0 to variant |> maxC do
                    if variant[offR,offC] = 1 then
                        row[rcToDlxCol (r+offR) (c+offC) boardWidth] <- 1
            // Mark the piece's id
            row[boardHeight * boardWidth + id] <- 1
            row
            )
        |> array2D |> printGridCustom printer
    
    // Try to solve the DLX matrix
    let dlx = Dlx()
    match dlx.Solve(matrix) |> Seq.tryHead with
    | Some solution ->
        true
        // printfn $"solution found : %A{solution.RowIndexes |> Seq.toArray}"
    | _ ->
        false
        //printfn "No solution found"

let square2_2 = Array2D.create 2 2 1
Check.That(canPlacePiecesOnBoard [| square2_2 |] (2,4)).IsEqualTo(true)
Check.That(canPlacePiecesOnBoard [| square2_2;square2_2 |] (2,4)).IsEqualTo(true)
Check.That(canPlacePiecesOnBoard [| square2_2; square2_2; square2_2 |] (2,4)).IsEqualTo(false)

let solve1 input =
    let regions, pieces = getInput input
    
    let (rows, cols), numPieces  = regions |> Seq.head
    
    let allPieces =
        numPieces
        |> Seq.indexed
        |> Seq.collect (fun (idx, num) -> Seq.replicate num pieces[idx])
        |> Seq.toArray
    
    canPlacePiecesOnBoard allPieces (rows, cols)
    
solve1 "Day12_sample2.txt"


    // let p1 = pieces[5]
    // enumerateValidPlacements p1 8 8
    // |> Seq.iter (fun (r,c) ->
    //     printfn ""
    //     let board = Array2D.zeroCreate<int> 8 8
    //     Array2D.blit p1 0 0 board r c (p1 |> Array2D.length1) (p1 |> Array2D.length2)
    //     board |> printGridCustom printer |> ignore)
    //  
    // getAllDistinctRotationsAndMirrors p1
    // |> Seq.iter (fun variation ->
    //     printfn ""
    //     variation |> printGridCustom printer |> ignore
    //     )
    