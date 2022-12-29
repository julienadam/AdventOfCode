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
    Blocks : (int*int) list
    MaxHeight : int
    Width : int
}

let minusShape = { Name = "Minus"; Blocks = [(0,0);(0,1);(0,2);(0,3)]; MaxHeight = 1; Width = 4 }
let plusShape = { Name = "Plus"; Blocks = [(0,1);(1,0);(1,1);(1,2);(2,1)]; MaxHeight = 3; Width = 3  }
let lShape = { Name = "L"; Blocks = [(0,0);(0,1);(0,2);(1,2);(2,2)]; MaxHeight = 3; Width = 3 }
let barShape = { Name = "Bar"; Blocks = [(0,0);(1,0);(2,0);(3,0)]; MaxHeight = 4; Width = 1}
let squareShape = { Name = "Square"; Blocks = [(0,0);(0,1);(1,0);(1,1)]; MaxHeight = 2; Width = 2}
let shapeSequence = [| minusShape; plusShape; lShape; barShape; squareShape |]

type Tunnel = {
    grid : List<byte>
    mutable highest : int
    heightMap : List<int>
} with 
    member this.GetRelativeHeightMap() =
        this.heightMap |> Seq.map(fun h -> this.highest - h) |> Seq.toArray
    member this.HasBlock r c =
        if r > this.grid.Count - 1 then
            false
        else
            let mask = (1uy <<< c)
            (mask &&& this.grid.[r]) = mask
    member this.AddBlock r c =
        if r = this.grid.Count then
            // Add a new line
            this.grid.Add (1uy <<< c)
        else if r < this.grid.Count then
            // Update existing line
            let mask = (1uy <<< c)
            this.grid.[r] <- (mask ||| this.grid.[r])
        else
            failwithf "Could not add row at %i. current height is %i" r this.grid.Count


let initTunnel() = 
    {
        grid = new List<byte>()
        highest = 0 
        heightMap = new List<int>([| 0;0;0;0;0;0;0 |])
    }

let display (t:Tunnel) = 
    for r in t.highest .. -1 .. 0  do
        printf "|"
        let bitmap = t.grid[r]
        for c = 0 to 6 do
            let check = (1uy <<< c)
            let isSet = (check &&& bitmap) = check
            printf "%c" (if isSet then '#' else '.')
        printfn "|"
    printfn "+-------+"

let doesShapeHitSomething shape (r,c) (tunnel:Tunnel) = 
    shape.Blocks |> Seq.exists(fun (br, bc) -> 
        tunnel.HasBlock (r + br) (c + bc)
    )
        
let moveShapeLeft shape (r,c) tunnel =
    if c = 0 then (r,c) 
    else if doesShapeHitSomething shape (r,c-1) tunnel then (r,c)
    else (r, c-1)

let moveShapeRight shape (r,c) tunnel =
    if c > (6 - shape.Width) then (r,c) 
    else if doesShapeHitSomething shape (r,c+1) tunnel then (r,c)
    else (r, c+1)
       
let canMoveShapeDown shape (r,c) tunnel =
    if r = 0 then false
    else 
        not (doesShapeHitSomething shape (r-1,c) tunnel)
       
let freezeShape shape (r,c) tunnel =
    let nh = max tunnel.highest (r + shape.MaxHeight - 1 )

    shape.Blocks |> Seq.iter (fun (br,bc) ->
        let nr, nc = (r + br), (c + bc)
        // Update grid
        tunnel.AddBlock nr nc |> ignore
        // Update heightmap
        if nr > tunnel.heightMap.[nc] then
            tunnel.heightMap.[nc] <- nr
    )

    tunnel.highest <- nh
    tunnel

type StateKey = {
    ShapeIndex : int
    JetIndex : int
    HeightMap : int[]
}

type StateValue = {
    RocksFallen : int64
    Highest : int
}

type StatesMap = Dictionary<StateKey, StateValue>

let loop maxRocks rockNb (r,c) tunnel (jets:Jet[]) (states:StatesMap) =
    let rec loopRec maxRocks rockNb shapeIndex (r,c) tunnel jetIndex =
        let actualJetIndex = jetIndex % jets.Length
        let jet = jets.[actualJetIndex]
        let actualShapeIndex = shapeIndex % shapeSequence.Length
        let shape = shapeSequence.[actualShapeIndex]
        let (rn,cn) = 
            match jet with
            | Jet.Left -> 
                moveShapeLeft shape (r,c) tunnel
            | Jet.Right -> 
                moveShapeRight shape (r,c) tunnel
        
        if canMoveShapeDown shape (rn, cn) tunnel then
            loopRec maxRocks rockNb shapeIndex (rn-1, cn) tunnel (jetIndex + 1)
        else
            let nextTunnel = freezeShape shape (rn, cn) tunnel
            let newState = { ShapeIndex = actualShapeIndex; JetIndex = actualJetIndex; HeightMap = tunnel.GetRelativeHeightMap() }
            
            // Check state map for a duplicate
            match states.TryGetValue(newState) with
            | true, similarPastState -> 
                printfn "Cycle found between %i and %i" similarPastState.RocksFallen rockNb
                let cycleHeight = int64(nextTunnel.highest - similarPastState.Highest)
                let cycleLength = (rockNb - similarPastState.RocksFallen)
                let remainingRocks = (maxRocks - similarPastState.RocksFallen) % int64(cycleLength)
                let numFullCycles = (maxRocks - similarPastState.RocksFallen) / int64(cycleLength)

                // Look in the cache to find the height after 
                // similarPastState.RocksFallen + remainder rocks dropped
                let outOfCycleState =
                    states 
                    |> Seq.find (fun kvp -> kvp.Value.RocksFallen = (similarPastState.RocksFallen + remainingRocks))

                let result =
                    int64(outOfCycleState.Value.Highest) +
                    numFullCycles * cycleHeight
                result
            | _ -> 
                // If not found, add the state to the map and continue
                states.Add(newState, { RocksFallen = rockNb; Highest = nextTunnel.highest })
                let pos = nextTunnel.highest + 3 + 1, 2
                loopRec maxRocks (rockNb + 1L) (shapeIndex + 1) pos nextTunnel (jetIndex + 1)
    loopRec maxRocks rockNb 0 (r,c) tunnel 0

let solve1 input =
    let instructions = getInput input
    let result = (loop 2023L 1L  (3, 2) (initTunnel()) instructions ) (new StatesMap())
    result

solve1 "day17.txt"

let solve2 input =
    let instructions = getInput input
    let result = (loop 1000_000_000_001L 1L (3, 2) (initTunnel()) instructions ) (new StatesMap())
    result

solve2 "day17.txt"
