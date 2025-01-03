namespace AdventOfCode

[<AutoOpen>]
module Directions =

    let inline (++) (a,b) (c,d) = (a+c, b+d)
    let inline mul (a,b) mult = (a*mult, b*mult)

    let north = (-1,0)
    let south = (1,0)
    let east = (0,1)
    let west = (0,-1)

    type Direction =
    | North
    | East
    | South
    | West
    with 
        member this.TurnLeft () = match this with | North -> West | East -> North | South -> East | West -> South
        member this.TurnRight () = match this with | North -> East | East -> South | South ->  West | West ->  North
        member this.Move(p:int*int) =
            match this with
            | North -> p ++ north
            | South -> p ++ south
            | East -> p ++ east
            | West -> p ++ west
        member this.MoveBy(p:int*int, dist:int) =
            match this with
            | North -> p ++ (mul north dist)
            | South -> p ++ (mul south dist)
            | East -> p ++ (mul east dist)
            | West -> p ++ (mul west dist)
        member this.GetDegrees() = 
            match this with
            | North -> 0
            | East -> 90
            | South -> 180
            | West -> 270
        static member FromDegrees(deg: int) =
            match deg % 360 with
            | 0 -> North
            | 90 -> East
            | 180 -> South
            | 270 -> West
            | _ -> failwithf "Invalid degrees for directions, must be a multiple of 90�, was %i" deg

    type Compass =
    | Up
    | Down
    | Left
    | Right
    | UpRight
    | UpLeft
    | DownRight
    | DownLeft