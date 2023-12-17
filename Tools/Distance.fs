namespace AdventOfCode

[<AutoOpen>]
module Distance =
    let inline manhattanDistance (x1: ^a) (y1: ^a) (x2: ^a) (y2: ^a) : ^a = (abs (x1 - x2)) + (abs (y1 - y2))
    let inline manhattanDistPoints (x1,y1) (x2,y2) = manhattanDistance x1 y1 x2 y2
