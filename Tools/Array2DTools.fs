namespace AdventOfCode

module Array2DTools =

    let getAdjacent row col (grid:'a[,]) = seq {
        if row > 0 then
            yield ((row - 1), col, grid.[(row - 1), col])
        if row < ((grid |> Array2D.length1) - 1) then
            yield ((row + 1), col, grid.[(row + 1), col])
        if col > 0 then
            yield (row, (col - 1), grid.[row, (col - 1)])
        if col < ((grid |> Array2D.length2) - 1) then
            yield (row, (col + 1), grid.[row, (col + 1)])
    }

    let getAdjacentCoords row col rowLength colLength = seq {
        if row > 0 then
            yield ((row - 1), col)
        if row < rowLength - 1 then
            yield ((row + 1), col)
        if col > 0 then
            yield (row, (col - 1))
        if col < colLength - 1 then
            yield (row, (col + 1))
    }

    let getAdjacentCoordsDiagUp row col rowLength colLength = seq {
        if row > 0 && col > 0 then
           yield ((row - 1), (col - 1))
        if row > 0 then
            yield ((row - 1), col)
        if row > 0 && col < colLength - 1 then
           yield ((row - 1), (col + 1))
    }

    let getAdjacentCoordsDiagLeftRight row col rowLength colLength = seq {
        if col > 0 then
            yield (row, (col - 1))
        if col < colLength - 1 then
            yield (row, (col + 1))
    }

    let getAdjacentCoordsDiagDown (row:int) (col:int) (rowLength:int) (colLength:int) = seq {
        if row < (rowLength - 1) && (col > 0) then
            yield ((row + 1), (col - 1))
        if row < rowLength - 1 then
            yield ((row + 1), col)
        if row <(rowLength - 1) && col < (colLength - 1) then
            yield ((row + 1), (col + 1))
    }
    
    let getAdjacentCoordsDiags row col rowLength colLength = seq {
        yield! getAdjacentCoordsDiagUp row col rowLength colLength
        yield! getAdjacentCoordsDiagLeftRight row col rowLength colLength
        yield! getAdjacentCoordsDiagDown row col rowLength colLength
    }

    // MISSING A DIAGONAL !
    //let getAdjacentWithDiagonals row col (grid:'a[,]) = seq {
    //    yield! getAdjacent row col (grid:'a[,])
    //    if row > 0 && col < ((grid |> Array2D.length2) - 1) then
    //        yield ((row - 1), (col + 1), grid.[(row - 1), (col + 1)])
    //    if row <((grid |> Array2D.length1) - 1) && col > 0 then
    //        yield ((row + 1), (col - 1), grid.[(row + 1), (col - 1)])
    //    if row <((grid |> Array2D.length1) - 1) && col < ((grid |> Array2D.length2) - 1) then
    //        yield ((row + 1), (col + 1), grid.[(row + 1), (col + 1)])
    //}

    let enumArray2d (array:'a[,]) = seq {
        for i = 0 to (array |> Array2D.length1) - 1 do
            for j = 0 to (array |> Array2D.length2) - 1 do
                yield i,j, array.[i,j]
    }

    let printGrid (grid:'a[,]) =
        for i in [0..grid.GetLength(0) - 1] do
            for j in [0..grid.GetLength(1) - 1] do
                printf "%O" grid.[i,j]
                // if j % 10 = 9 then
                //     printf " "
            printfn ""
            // if i % 10 = 9 then
            //     printfn ""
    
    let printGridCustom dataToChar (grid:'a[,]) =
        for i in [0..grid.GetLength(0) - 1] do
            for j in [0..grid.GetLength(1) - 1] do
                printf "%c" (dataToChar grid.[i,j])
                // if j % 10 = 9 then
                //     printf " "
            printfn ""
            // if i % 10 = 9 then
            //     printfn ""
        grid

    let inline lenR grid = (grid |> Array2D.length1)
    let inline lenC grid = (grid |> Array2D.length2)
    let inline maxR grid = (lenR grid) - 1
    let inline maxC grid = (lenC grid) - 1
    let inline transpose grid = Array2D.init (grid |> lenC) (grid |> lenR) (fun r c -> grid[c,r])