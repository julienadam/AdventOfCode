#time
#load "../../Tools.fsx"

open System.IO
open System.Collections.Generic
open Checked
open Tools

type Jet = | Left | Right


let getInput p = 
    File.ReadAllText(getInputPath2022 p)
    |> Seq.map (fun c -> match c with | '<' -> Jet.Left | '>' -> Jet.Right | _ -> failwithf "Invalid input %c" c)
    |> Seq.toArray
    |> Dump

type Shape = {
    Name : string
    Blocks : (int64*int) list
    MaxHeight : int64
    HeightMap : int[]
}

let minusShape = { Name = "Minus"; Blocks = [(0,0);(0,1);(0,2);(0,3)]; MaxHeight = 1; HeightMap = [|1;1;1;1|] }
let plusShape = { Name = "Plus"; Blocks = [(0,1);(1,0);(1,1);(1,2);(2,1)]; MaxHeight = 3; HeightMap = [|2;3;2|]  }
let lShape = { Name = "L"; Blocks = [(0,0);(0,1);(0,2);(1,2);(2,2)]; MaxHeight = 3; HeightMap = [|1;1;3|] }
let barShape = { Name = "Bar"; Blocks = [(0,0);(1,0);(2,0);(3,0)]; MaxHeight = 4; HeightMap = [|4|] }
let squareShape = { Name = "Square"; Blocks = [(0,0);(0,1);(1,0);(1,1)]; MaxHeight = 2; HeightMap = [|2;2|]  }
let shapeSequence = [| minusShape; plusShape; lShape; barShape; squareShape |]

type Tunnel = {
    grid : HashSet<int64*int>
    mutable highest : int64
    heightMap : List<int64>
} with member this.GetRelativeHeightMap() =
        this.heightMap |> Seq.map(fun h -> this.highest - h) |> Seq.toArray

let initTunnel() = 
    {
        grid = new HashSet<int64*int>()
        highest = 0L 
        heightMap = new List<int64>([| 0L;0;0;0;0;0;0 |])
    }

let display (t:Tunnel) = 
    for r in t.highest .. -1L .. 0L  do
        printf "|"
        for c = 0 to 6 do
            printf "%c" (if t.grid.Contains(r,c) then '#' else '.')
        printfn "|"
    printfn "+-------+"

let doesShapeHitSomething shape (r,c) tunnel = 
    shape.Blocks |> Seq.exists(fun (br, bc) -> 
        tunnel.grid.Contains(r + br, c + bc)
    )
        
let moveShapeLeft shape (r,c) tunnel =
    if c = 0 then (r,c) 
    else if doesShapeHitSomething shape (r,c-1) tunnel then (r,c)
    else (r, c-1)

let moveShapeRight shape (r,c) tunnel =
    if c > (6 - shape.HeightMap.Length) then (r,c) 
    else if doesShapeHitSomething shape (r,c+1) tunnel then (r,c)
    else (r, c+1)
       
let canMoveShapeDown shape (r,c) tunnel =
    if r = 0L then false
    else 
        not (doesShapeHitSomething shape (r-1L,c) tunnel)
       
let freezeShape shape (r,c) tunnel =
    let nh = max tunnel.highest (r + shape.MaxHeight - 1L )
    //printfn "new highest %i" nh

    shape.Blocks |> Seq.iter (fun (br,bc) ->
        let nr, nc = (r + br), (c + bc)
        // Update grid
        tunnel.grid.Add(nr,nc) |> ignore
        // Update heightmap
        if nr > tunnel.heightMap.[nc] then
            tunnel.heightMap.[nc] <- nr
    )

    tunnel.highest <- nh
    tunnel

let nt = 
    initTunnel()
    |> freezeShape plusShape (0L,1) 
    |> freezeShape minusShape (3L,3) 
nt.GetRelativeHeightMap() |> Dump
nt |> display

type StateKey = {
    ShapeIndex : int
    JetIndex : int
    HeightMap : int64[]
}

type StateValue = {
    RocksFallen : int64
    Highest : int64
}

type StatesMap = Dictionary<StateKey, StateValue>


let loop maxRocks rockNb shapeIndex (r,c) tunnel (jets:Jet[]) index (states:StatesMap) =
    let rec loopRec maxRocks rockNb shapeIndex (r,c) tunnel index =
        if rockNb = maxRocks then
            tunnel
        else
            let jet = jets.[index % jets.Length]
            let shape = shapeSequence.[shapeIndex % shapeSequence.Length]
            let (rn,cn) = 
                match jet with
                | Jet.Left -> 
                    moveShapeLeft shape (r,c) tunnel
                | Jet.Right -> 
                    moveShapeRight shape (r,c) tunnel
        
            if canMoveShapeDown shape (rn, cn) tunnel then
                loopRec maxRocks rockNb shapeIndex (rn-1L, cn) tunnel (index + 1)
            else
                if rockNb % 1_000_000L = 0L then
                    printfn "Rock #%i out of %i falling, states map has %i" rockNb maxRocks states.Count
            
                let nextTunnel = freezeShape shape (rn, cn) tunnel
                let newState = { ShapeIndex = shapeIndex; JetIndex = index; HeightMap = tunnel.GetRelativeHeightMap() }
            
                // Check state map for a duplicate
                match states.TryGetValue(newState) with
                | true, similarPastState -> 
                    printfn "Cycle found between %i and %i" similarPastState.RocksFallen rockNb
                    failwithf "Have to compute"
                | _ -> 
                    // If not found, add the state to the map and continue
                    states.Add(newState, { RocksFallen = rockNb; Highest = tunnel.highest })
                    let pos = nextTunnel.highest + 3L + 1L, 2
                    loopRec maxRocks (rockNb + 1L) (shapeIndex + 1) pos nextTunnel (index + 1)
    loopRec maxRocks rockNb shapeIndex (r,c) tunnel index

let solve1 input =
    let instructions = getInput input
    let result = (loop 2023L 1L 0 (3, 2) (initTunnel()) instructions 0) (new StatesMap())
    result |> display
    result.highest + 1L

// solve1 "day17.txt"

let solve2 input =
    let instructions = getInput input
    let result = (loop 1000_000_000_001L 1L 0 (3, 2) (initTunnel()) instructions 0) (new StatesMap())
    result.highest + 1L

solve2 "day17_sample1.txt"

// Not going to bruteforce it
// TODO Record state (jet index, shape index, height map for each column (adjusted to 0)
// TODO Find a cycle, compute the cycle state closest to the end
// TODO then let the rest play normally

(File.ReadAllText(getInputPath2022 "Day17.txt")).Length