namespace AdventOfCode

[<AutoOpen>]
module Distance =
    open System
    let inline manhattanDistance (x1:int) (y1:int) (x2:int) (y2:int) = Math.Abs(x1 - x2) + Math.Abs(y1 - y2)
    let inline manhattanDistPoints (x1,y1) (x2,y2) = manhattanDistance x1 y1 x2 y2
