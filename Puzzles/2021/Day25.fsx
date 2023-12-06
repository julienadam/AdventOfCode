#load "../../Tools.fs"
#load "../../Tools/Array2DTools.fs"
#time "on"

open System
open System.IO
open AdventOfCode

type Contents =
    | Nothing
    | EastCuke
    | SouthCuke

let readChar = 
    function 
    | '>' -> EastCuke 
    | 'v' -> SouthCuke 
    | '.' -> Nothing 
    | _ -> failwithf "Invalid input char"

let writeChar = 
    function 
    | EastCuke  -> '>'
    | SouthCuke -> 'v' 
    | Nothing  -> '.'
    
let getInput fileName = 
    getInputPath fileName
    |> File.ReadAllLines 
    |> Seq.map (fun l -> l |> Seq.map readChar)
    |> array2D

let printSeaFloor seaFloor = 
    for i = 0 to (seaFloor |> Array2D.length1) - 1 do
        for j = 0 to (seaFloor |> Array2D.length2) - 1 do
            printf "%c" (writeChar seaFloor.[i,j])
        printfn ""

let step seaFloor =
    let moveEast () =
        let mutable cukesMovedEast = 0
        // Move East cukes
        let cukesToMoveEast = 
            seaFloor 
            |> Array2DTools.enumArray2d
            |> Seq.choose (fun (row,col,v) ->
                let targetCol = ((col + 1) % (seaFloor |> Array2D.length2))
                match v, seaFloor.[row, targetCol] with 
                | EastCuke, Nothing -> Some (row, col, targetCol)
                | _ -> None)
            |> Seq.toList

        cukesToMoveEast |> Seq.iter( fun (row, col, targetCol) -> 
            cukesMovedEast <- cukesMovedEast + 1
            seaFloor.[row, col] <- Nothing
            seaFloor.[row, targetCol] <- EastCuke)
        cukesMovedEast


    let moveSouth () =
        let mutable cukesMovedSouth = 0
        let cukesToMoveSouth = 
            seaFloor 
            |> Array2DTools.enumArray2d
            |> Seq.choose (fun (row,col,v) ->
                let targetRow = ((row + 1) % (seaFloor |> Array2D.length1))
                match v, seaFloor.[targetRow, col] with 
                | SouthCuke, Nothing -> Some (row, targetRow, col)
                | _ -> None)
            |> Seq.toList

        cukesToMoveSouth |> Seq.iter( fun (row, targetRow, col) -> 
            cukesMovedSouth <- cukesMovedSouth + 1
            seaFloor.[row, col] <- Nothing
            seaFloor.[targetRow, col] <- SouthCuke)
            
        cukesMovedSouth

    moveEast() + moveSouth()
    
let rec solve seaFloor iteration =
    //printfn ""
    //printfn "Iteration %i" iteration
    //printSeaFloor seaFloor
    if step seaFloor = 0 then
        iteration
    else
        solve seaFloor (iteration + 1)

let input = (getInput "Day25.txt")
solve input 1 