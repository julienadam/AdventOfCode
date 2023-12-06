namespace AdventOfCode

type Direction =
| North
| East
| South
| West
with 
    member this.TurnLeft () = match this with | North -> West | East -> North | South -> East | West -> South
    member this.TurnRight () = match this with | North -> East | East -> South | South ->  West | West ->  North

type Compass =
| Up
| Down
| Left
| Right
| UpRight
| UpLeft
| DownRight
| DownLeft