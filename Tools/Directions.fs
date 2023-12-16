namespace AdventOfCode

[<AutoOpen>]
module Directions =

    let inline (++) (a,b) (c,d) = (a+c, b+d)
    let private north = (-1,0)
    let private south = (1,0)
    let private east = (0,1)
    let private west = (0,-1)

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

    type Compass =
    | Up
    | Down
    | Left
    | Right
    | UpRight
    | UpLeft
    | DownRight
    | DownLeft