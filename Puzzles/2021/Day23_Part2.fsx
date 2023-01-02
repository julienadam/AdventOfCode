#load "../../Tools.fs"
#time "on"

open System
open System.Diagnostics
open System.Collections.Generic
open System.IO
open AdventOfCode
open AdventOfCode.AStar

type Amphipod = 
    // | Amber = 1 | Bronze = 10 | Copper = 100  | Desert = 1000
    | Amber = 2 | Bronze = 4 | Copper = 6  | Desert = 8

let Left_back = 0
let A = 2 
let B = 4
let C = 6
let D = 8
let Right_back = 10

let readAmphipod c =
    match c with
    | 'A' -> Amphipod.Amber 
    | 'B' -> Amphipod.Bronze
    | 'C' -> Amphipod.Copper
    | 'D' -> Amphipod.Desert
    | _ -> failwithf "Invalid amphipod type"

// Possible state representations :
// mutable array starting with 4xRoomSize then Corridor
// 2 mutable arrays for rooms and corridor

type RoomState = {
    Position : int
    Type : Amphipod
    Depth: int
    Inhabitants : Amphipod list
    CanReceive : bool
} with
    member this.AddInhabitant(a:Amphipod) =
        if a <> this.Type then
            failwithf "Invalid amphipod for room"
        if this.Inhabitants.Length >= 4 then
            failwithf "Already full"
        { this with Inhabitants = a :: this.Inhabitants }
    member this.AddInitialInhabitant(a:Amphipod) =
        { 
            this with 
                Inhabitants = a :: this.Inhabitants
                CanReceive = this.CanReceive && a = this.Type
        }
    member this.AddInitialOptionalInhabitant((a:Amphipod), depth) =
        if depth = 4 then
            this.AddInitialInhabitant a
        else 
            this
    member this.MoveOut() =
        { 
            this with 
                Inhabitants = this.Inhabitants.Tail
                CanReceive = this.Inhabitants.Tail |> Seq.forall (fun i -> i = this.Type)
        }
    member this.Peek() = this.Inhabitants.Head
    member this.DistanceToMove = this.Depth - this.Inhabitants.Length
    member this.IsDone = this.CanReceive && this.Depth = this.Inhabitants.Length

let inline createRoom typ depth = { Position = typ |> int; Type = typ; Depth = depth; Inhabitants = []; CanReceive = true }

//let fullRoom = 
//    (createRoom Amphipod.Amber)
//        .AddInitialInhabitant(Amphipod.Amber)
//        .AddInitialInhabitant(Amphipod.Bronze)
//        .MoveOut()
//        .AddInhabitant(Amphipod.Amber)
//        .AddInhabitant(Amphipod.Amber)
//        .AddInhabitant(Amphipod.Amber)

type BurrowArrayState = {
    RoomA: RoomState
    RoomB: RoomState
    RoomC: RoomState
    RoomD: RoomState
    Corridor: Amphipod option[]
} with 
    member this.GetRoom roomPos =
        match roomPos with
        | 2 -> this.RoomA
        | 4 -> this.RoomB
        | 6 -> this.RoomC
        | 8 -> this.RoomD
        | _ -> failwithf ""
    member this.MoveFromRoomToCorridor(roomPos, corridorPos) =
        let newCorridors = this.Corridor |> Array.copy
        let r = this.GetRoom roomPos
        newCorridors.[corridorPos] <- r.Peek() |> Some
        match roomPos with
        | 2 -> { this with RoomA = r.MoveOut(); Corridor = newCorridors}
        | 4 -> { this with RoomB = r.MoveOut(); Corridor = newCorridors}
        | 6 -> { this with RoomC = r.MoveOut(); Corridor = newCorridors}
        | 8 -> { this with RoomD = r.MoveOut(); Corridor = newCorridors}
        | _ -> failwithf ""
    member this.MoveFromCorridorToRoom(corridorPos, amphipod) =
        let roomPos = amphipod |> int
        let newCorridors = this.Corridor |> Array.copy
        newCorridors.[corridorPos] <- None
        let r = this.GetRoom roomPos
        match roomPos with
        | 2 -> { this with RoomA = r.AddInhabitant(amphipod); Corridor = newCorridors}
        | 4 -> { this with RoomB = r.AddInhabitant(amphipod); Corridor = newCorridors}
        | 6 -> { this with RoomC = r.AddInhabitant(amphipod); Corridor = newCorridors}
        | 8 -> { this with RoomD = r.AddInhabitant(amphipod); Corridor = newCorridors}
        | _ -> failwithf ""
    member this.Rooms = [this.RoomA; this.RoomB; this.RoomC; this.RoomD;]

let getInput depth fileName = 
    let lines = getInputPath fileName |> File.ReadAllLines 
    let filledRoomA = 
        (createRoom Amphipod.Amber depth)
            .AddInitialInhabitant(readAmphipod lines.[3].[3])
            .AddInitialOptionalInhabitant(Amphipod.Desert,depth)
            .AddInitialOptionalInhabitant(Amphipod.Desert,depth)
            .AddInitialInhabitant(readAmphipod lines.[2].[3])
        
    let filledRoomB = 
           (createRoom Amphipod.Bronze depth)
            .AddInitialInhabitant(readAmphipod lines.[3].[5])
            .AddInitialOptionalInhabitant(Amphipod.Bronze,depth)
            .AddInitialOptionalInhabitant(Amphipod.Copper,depth)
            .AddInitialInhabitant(readAmphipod lines.[2].[5])
    let filledRoomC = 
        (createRoom Amphipod.Copper depth)
            .AddInitialInhabitant(readAmphipod lines.[3].[7])
            .AddInitialOptionalInhabitant(Amphipod.Amber ,depth)
            .AddInitialOptionalInhabitant(Amphipod.Bronze,depth)
            .AddInitialInhabitant(readAmphipod lines.[2].[7])
    let filledRoomD = 
        (createRoom Amphipod.Desert depth)
            .AddInitialInhabitant(readAmphipod lines.[3].[9])
            .AddInitialOptionalInhabitant(Amphipod.Copper,depth)
            .AddInitialOptionalInhabitant(Amphipod.Amber,depth)
            .AddInitialInhabitant(readAmphipod lines.[2].[9])
    { 
        RoomA = filledRoomA; RoomB = filledRoomB; RoomC = filledRoomC; RoomD = filledRoomD
        Corridor = [|None;None;None;None;None;None;None;None;None;None;None|]
    }

let printState state =
    let getAmphipodDisplay a =
        match a with 
        | Some x -> 
            match x with 
            | Amphipod.Amber  -> 'A'
            | Amphipod.Bronze -> 'B'
            | Amphipod.Copper -> 'C'
            | Amphipod.Desert -> 'D'
            | _ -> failwithf "invalid enum"
        | None -> '.'
    let roomDisplay room = 
        List.concat [
            List.init (room.Depth - room.Inhabitants.Length) (fun _ -> '.')
            room.Inhabitants |> List.map (Some >> getAmphipodDisplay)]

    let aDisplay = roomDisplay state.RoomA
    let bDisplay = roomDisplay state.RoomB
    let cDisplay = roomDisplay state.RoomC
    let dDisplay = roomDisplay state.RoomD
    
    printfn "#############"
    printf "#"
    state.Corridor |> Seq.iter (fun a -> printf "%c" (getAmphipodDisplay a))
    printfn "#"
    printfn "###%c#%c#%c#%c###" aDisplay.[0] bDisplay.[0] cDisplay.[0] dDisplay.[0]
    for i = 1 to state.RoomA.Depth - 1 do
        printfn "  #%c#%c#%c#%c#" aDisplay.[i] bDisplay.[i] cDisplay.[i] dDisplay.[i]
    printfn "  #########"
    
let moveCost = function
    | Amphipod.Amber -> 1
    | Amphipod.Bronze -> 10
    | Amphipod.Copper -> 100
    | Amphipod.Desert -> 1000
    | _ -> failwithf "Invalid amphipod type"
    
let findCostToEnterRoom c (amphipod:Amphipod) (state:BurrowArrayState) =
    // Check that the room is indeed free before all else
    let r = state.GetRoom (amphipod |> int)
    if not r.CanReceive then
        None
    else
        let mutable blocked = false
        let mutable x = c
        let mutable result = None
        if c < r.Position then
            while (not blocked && x < r.Position) do
                x <- x + 1
                if x % 2 = 1 && state.Corridor[x].IsSome then
                    blocked <- true
                if x = r.Position then
                    result <- Some ((r.DistanceToMove + Math.Abs(c - r.Position)) * (moveCost amphipod))
        else if c > r.Position then
            while (not blocked && x > r.Position) do
                x <- x - 1
                if x % 2 = 1 && state.Corridor[x].IsSome then
                    blocked <- true
                if x = r.Position then
                    result <- Some ((r.DistanceToMove + Math.Abs(c - r.Position)) * (moveCost amphipod))
        else 
            failwithf "Invariant failure : target at same coords as corridor spot"
        result


let state = 
    (getInput 2 "Day23_sample1.txt")
        .MoveFromRoomToCorridor(4,10)
        .MoveFromRoomToCorridor(4,0)
        .MoveFromRoomToCorridor(6,9)

// state |> printState

assert(findCostToEnterRoom 9 Amphipod.Bronze state = Some 70)


let findFreeCorridorsRight roomPos state = seq {
    let mutable blocked = false
    let mutable x = roomPos
    while (not blocked && x < Right_back) do
        x <- x + 1
        if x % 2 = 1 || x = Right_back then // Corridor
            if state.Corridor[x].IsSome then
                blocked <- true
            else 
                yield x
}

let findFreeCorridorsLeft roomPos state = seq {
    let mutable blocked = false
    let mutable x = roomPos
    while (not blocked && x > Left_back) do
        x <- x - 1
        if x % 2 = 1 || x = Left_back  then // Corridor
            if state.Corridor[x].IsSome then
                blocked <- true
            else 
                yield x
}

let calculateRoomToCorridorCost room corridorPos =
    let dist = Math.Abs(corridorPos - room.Position) + 1 + room.DistanceToMove
    let cost = (moveCost (room.Peek())) * dist
    cost

//state |> printState
//calculateRoomToCorridorCost state.RoomA 7

let getNextStates (state:BurrowArrayState) = seq {
    // Rooms that still have other amphipods have to be emptied of other amphipods
    let roomsToHandle = state.Rooms |> Seq.filter (fun r -> not r.IsDone && not r.CanReceive) 
    for r in roomsToHandle do
        yield! findFreeCorridorsRight r.Position state |> Seq.map (fun x -> 
            let cost = calculateRoomToCorridorCost r x
            state.MoveFromRoomToCorridor(r.Position, x), cost
        )
        yield! findFreeCorridorsLeft r.Position state |> Seq.map (fun x -> 
            let cost = calculateRoomToCorridorCost r x
            state.MoveFromRoomToCorridor(r.Position, x), cost
        )

    // Corridor -> room
    for i = 0 to 10 do
        match state.Corridor.[i] with
        | None -> ()
        | Some a -> 
            match findCostToEnterRoom i a state with
            | Some cost -> 
                yield state.MoveFromCorridorToRoom(i,a), cost
            | None -> ()
}
    
//let heuristic (burrow: BurrowState) =
//    // Compute theorical distance if there were no other amphipods on the grid
//    burrow.ActiveAmphipods 
//    |> List.map (fun (amphipod, loc) ->
//        let t = getAssignedRoom amphipod
//        let d = absDist (room t 2) loc 
//        assert (d >= 0) 
//        amphipod, d
//        )
//    |> List.groupBy fst
//    |> List.collect (fun (amphipodType, dists) -> 
//        dists 
//        |> Seq.sortBy snd
//        |> Seq.mapi (fun i (_,d) -> computeCost ((d - i + 1) |> int64) amphipodType)
//        |> Seq.toList
//    )
//    |> Seq.sum
    
let inline isOrganizedBurrow state =
    state.RoomA.IsDone && state.RoomB.IsDone && state.RoomC.IsDone && state.RoomD.IsDone

let mutable openCount = 0
let mutable closedCount = 0
let mutable iterations = 0
let rec pathfindingAStarRec (openNodes:Dictionary<BurrowArrayState, AStarNode<BurrowArrayState>>) (closedNodes:Set<BurrowArrayState>) =
    openCount <- openNodes.Count
    iterations <- iterations + 1
    if iterations % 1000 = 0 then 
        printfn "Iteration %i. Open %i Closed %i" openCount iterations closedCount
    if openNodes.Count = 0 then
        failwithf "No path found"
    else
        let bestNode = openNodes.Values |> Seq.minBy (fun n -> n.F)
        //printfn "Selecting state %A" bestNode.Data
        //printState bestNode.Data
        //let sw = Stopwatch.StartNew()
        let isFinalState = isOrganizedBurrow bestNode.Data
        //printfn "Final state checking took %A" sw.Elapsed
        if isFinalState then
            printfn "Found optimal path with cost %i" bestNode.G
            bestNode.G
        else
            let closed = Set.add bestNode.Data closedNodes 
            closedCount <- closedCount + 1
            // Todo, mark removed instead ?
            if openNodes.Remove(bestNode.Data) |> not then
                failwithf "Should never fail, we just took it from the list"

            //sw.Restart()
            let adjacentNonClosed = 
                getNextStates bestNode.Data 
                |> Seq.filter (fun (newNode,_) -> closedNodes |> Set.contains newNode |> not)
            //printfn "Finding next nodes took %A" sw.Elapsed
            //printfn "Found %i possible targets" adjacentNonClosed.Length

            adjacentNonClosed 
            |> Seq.iter (fun (newNode, cost) ->
                // Calculate total cost of new node
                let g = bestNode.G + (cost |> int64)

                // Heuristic cost towards final state
                //sw.Restart()
                //let h = heuristic newNode
                // Disabled heuristics
                let h = 0L
                //printfn "Heuristic took %A" sw.Elapsed
                if h < 0 then
                    failwithf "Invariant failure : heuristic is negative. %i" h
                //printfn "Heuristic %i" h
                
                // If this state already exists with a lower cost, skip it
                
                match openNodes.TryGetValue(newNode) with 
                | true, n when g > n.G -> ()
                | _ -> openNodes.[newNode] <- { Data = newNode; F = h + g; G = g }
            )
            
            //let nextOpen = Seq.concat [nextOpenNodes ; remainingOpenNodes]
            pathfindingAStarRec openNodes closed 

let solve1 () = 
    let input = getInput 2 "Day23.txt"
    let d = new Dictionary<BurrowArrayState, AStarNode<BurrowArrayState>>()
    d.Add(input, { Data = input; G = 0; F = 0 })
    pathfindingAStarRec d Set.empty

let solve2 () =
    let input = getInput 4 "Day23.txt"
    let d = new Dictionary<BurrowArrayState, AStarNode<BurrowArrayState>>()
    d.Add(input, { Data = input; G = 0; F = 0 })
    pathfindingAStarRec d Set.empty
