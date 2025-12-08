namespace AdventOfCode

open System

[<AutoOpen>]
module Distance =
    let inline manhattanDistance (x1: ^a) (y1: ^a) (x2: ^a) (y2: ^a) : ^a = (abs (x1 - x2)) + (abs (y1 - y2))
    let inline manhattanDistPoints (x1,y1) (x2,y2) = manhattanDistance x1 y1 x2 y2
    let inline manhattanDistance3D (x1: ^a) (y1: ^a) (z1: ^a) (x2: ^a) (y2: ^a) (z2: ^a): ^a = (abs (x1 - x2)) + (abs (y1 - y2)) + (abs (z1 - z2))
    let inline manhattanDistPoints3D (x1,y1,z1) (x2,y2,z2) = manhattanDistance3D x1 y1 z1 x2 y2 z2
    
    let euclidianDistance3D (x1:int64,y1:int64,z1:int64) (x2:int64,y2:int64,z2:int64) =
        Math.Sqrt(double ((x1-x2)*(x1-x2)+(y1-y2)*(y1-y2)+(z1-z2)*(z1-z2)))
        
    let inline euclidianDistance3DSquared (x1,y1,z1) (x2,y2,z2) =
        ((x1-x2)*(x1-x2)+(y1-y2)*(y1-y2)+(z1-z2)*(z1-z2))