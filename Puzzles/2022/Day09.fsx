#load "../../Tools.fsx"
#r "nuget: XPlot.Plotly" 

open System
open System.IO
open System.Collections.Generic
open Tools

let mapLine (dir : string, len) = 
  let c = match dir.[0] with | 'R' -> Compass.Right | 'L' -> Compass.Left | 'U' -> Compass.Up | 'D' -> Compass.Down
  c, len |> Int32.Parse

let getInput p = File.ReadAllLines(getInputPath2022 p) |> Seq.map (split2 ' ') |> Seq.map mapLine

type coords = int * int


let move (x,y) (px,py) =
    let xMoves = (px > x + 1 || px < x - 1)
    let yMoves = (py > y + 1 || py < y - 1)
    
    let nx = 
        if px > x + 1 || ((px = x + 1) && yMoves) then
            x + 1
        else if px < x - 1 || ((px = x - 1) && yMoves) then
            x - 1
        else 
            x

    let ny = 
        if (py > y + 1) || ((py = y + 1) && xMoves) then
            y + 1
        else if py < y - 1 || ((py = y - 1) && xMoves) then
            y - 1
        else 
            y

    nx, ny


// Tests
//move (0,0) (1,0)
//move (0,0) (1,1)
//move (0,0) (0,2)
//move (0,0) (2,0)
//move (0,0) (2,2)
//move (0,0) (0,-2)
//move (0,0) (-2,0)
//move (0,0) (-2,-2)
//move (0,0) (-2,2)
//move (0,0) (2,-2)
//move (0,0) (2,1)
//move (0,0) (1,2)
//move (0,0) (1,-2)
//move (0,0) (-2, 1)
//move (0,0) (-2, -1)
//move (0,0) (-1, 2)
//move (0,0) (-1, -2)

type Knot = 
    {
        Name : string
        mutable Coords : coords
        mutable Previous: Knot option
        mutable Next: Knot option
        visitor: (coords -> unit) option
    } 
    member this.Move(dir: Compass) = 
        let x, y = this.Coords
        let nx, ny = 
            match dir with
            | Compass.Up -> (x, y-1)
            | Compass.Down -> (x, y+1)
            | Compass.Left -> (x-1, y)
            | Compass.Right -> (x+1, y)
            | _ -> failwithf "Not a valid direction"
        this.Coords <- nx, ny
        if this.Next.IsSome then
            this.Next.Value.Update()
    member this.Update() =
        let x, y = this.Coords
        let px, py = this.Previous.Value.Coords
        let nx, ny = move (x,y) (px,py)

        if not (this.Coords = (nx, ny)) then
            if this.visitor.IsSome then
                this.visitor.Value(nx, ny)
            this.Coords <- nx, ny
            if this.Next.IsSome then
                this.Next.Value.Update()

let solve1 input =
    let instructions = getInput input |> Seq.toList
    let visited = new HashSet<coords>([(0,0)])
    let visitor c = visited.Add(c) |> ignore

    let head = { Name = "Head"; Coords = (0,0); Previous = None; Next = None; visitor = None }
    let tail = { Name = "Tail"; Coords = (0,0); Previous = Some head; Next = None; visitor = Some visitor }
    head.Next <- Some tail

    instructions |> Seq.iter(fun (dir, len) -> 
        [1..len] |> Seq.iter(
            fun _ -> 
                head.Move dir
        )
    )

    visited.Count


solve1 "Day09.txt"


let solve2 input =
    let instructions = getInput input |> Seq.toList
    let visited = new HashSet<coords>([(0,0)])
    let visitor c = visited.Add(c) |> ignore

    let head = { Name = "Head"; Coords = (0,0); Previous = None; Next = None; visitor = None }
    let k1 = { Name = "1"; Coords = (0,0); Previous = Some head; Next = None; visitor = None }
    let k2 = { Name = "2"; Coords = (0,0); Previous = Some k1; Next = None; visitor = None }
    let k3 = { Name = "3"; Coords = (0,0); Previous = Some k2; Next = None; visitor = None }
    let k4 = { Name = "4"; Coords = (0,0); Previous = Some k3; Next = None; visitor = None }
    let k5 = { Name = "5"; Coords = (0,0); Previous = Some k4; Next = None; visitor = None }
    let k6 = { Name = "6"; Coords = (0,0); Previous = Some k5; Next = None; visitor = None }
    let k7 = { Name = "7"; Coords = (0,0); Previous = Some k6; Next = None; visitor = None }
    let k8 = { Name = "8"; Coords = (0,0); Previous = Some k7; Next = None; visitor = None }
    let tail = { Name = "Tail"; Coords = (0,0); Previous = Some k8; Next = None; visitor = Some visitor }
    head.Next <- Some k1
    k1.Next <- Some k2
    k2.Next <- Some k3
    k3.Next <- Some k4
    k4.Next <- Some k5
    k5.Next <- Some k6
    k6.Next <- Some k7
    k7.Next <- Some k8
    k8.Next <- Some tail
    
    instructions |> Seq.iter(fun (dir, len) -> 
        [1..len] |> Seq.iter(
            fun _ -> 
                head.Move dir
        )
    )

    visited.Count

solve2 "Day09.txt"