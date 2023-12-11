namespace AdventOfCode

[<AutoOpen>]
module Distance =
    let inline manhattanDistance (x1) (y1) (x2) (y2) = (abs (x1 - x2)) + (abs (y1 - y2))
    let inline manhattanDistPoints (x1,y1) (x2,y2) = manhattanDistance x1 y1 x2 y2
