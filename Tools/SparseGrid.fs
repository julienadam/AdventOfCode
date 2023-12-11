namespace AdventOfCode

module SparseGrid =
    open System.Collections.Generic

    let tryGet (row,col) (grid:IDictionary<int*int, 'a>) =
        match grid.TryGetValue((row,col)) with
        | true, v -> Some v
        | false, _ -> None

    let getAdjacent (row,col) (grid:IDictionary<int*int, 'a>) = seq {
        yield ((row - 1), col, grid |> tryGet (row - 1, col))
        yield ((row + 1), col, grid |> tryGet ((row + 1), col))
        yield (row, (col - 1), grid |> tryGet (row, (col - 1)))
        yield (row, (col + 1), grid |> tryGet (row, (col + 1)))
    }

    let getAdjacentWithDiagonals (row,col) grid = seq {
        yield! getAdjacent (row,col) grid
        yield ((row - 1), (col + 1), grid |> tryGet ((row - 1), (col + 1)))
        yield ((row - 1), (col - 1), grid |> tryGet ((row - 1), (col - 1)))
        yield ((row + 1), (col - 1), grid |> tryGet ((row + 1), (col - 1)))
        yield ((row + 1), (col + 1), grid |> tryGet ((row + 1), (col + 1)))
    }

    let minR (grid:IDictionary<int*int, 'a>) = grid.Keys |> Seq.map fst |> Seq.min
    let maxR (grid:IDictionary<int*int, 'a>) = grid.Keys |> Seq.map fst |> Seq.max
    let minC (grid:IDictionary<int*int, 'a>) = grid.Keys |> Seq.map snd |> Seq.min
    let maxC (grid:IDictionary<int*int, 'a>) = grid.Keys |> Seq.map snd |> Seq.max

    let printGrid dataToChar (grid:IDictionary<int*int, 'a>)=
        for r = (minR grid) to (maxR grid) do
            for c = (minC grid) to (maxC grid) do
                printf "%c" (dataToChar (grid |> tryGet (r,c)))
            printfn ""
        grid