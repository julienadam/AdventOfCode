#load "../../Tools.fs"

open System
open System.Diagnostics
open System.Collections.Generic
open System.IO
open AdventOfCode
open AdventOfCode.AStar

type Amphipod = | Amber = 1 | Bronze = 10 | Copper = 100  | Desert = 1000

let readAmphipod c =
    match c with
    | 'A' -> Amphipod.Amber 
    | 'B' -> Amphipod.Bronze
    | 'C' -> Amphipod.Copper
    | 'D' -> Amphipod.Desert
    | _ -> failwithf "Invalid amphipod type"

let Left_back = 0
let A = 2 
let B = 4
let C = 6
let D = 8
let Right_back = 10

type Loc =
    | Corridor of int
    | Room of int * int

type BurrowState = {
    Locations : Map<Loc, Amphipod>
    ActiveAmphipods : (Amphipod * Loc) list
}

let inline room r d = Room (r, d)
//let inline room r d = 
//    if d = 1 || d = 2 then 
//        if r = A || r = B || r = C || r = D then
//            Room (r, d) 
//        else
//            failwithf "Invalid room %i" r
//    else 
//        failwithf "Invalid depth %i" d

let inline corridor c = Corridor c
//let inline corridor c = 
//    if c >= Left_back && c <= Right_back && c <> A && c <> B && c <> C && c <> D then
//        Corridor c
//    else
//        failwithf "Invalid corridor %i" c
    
let inline isColorOpt type' = 
    function 
    | Some a when a = type' -> true 
    | _ -> false
    
let getAssignedRoom = 
    function 
    | Amphipod.Amber -> A 
    | Amphipod.Bronze -> B 
    | Amphipod.Copper -> C
    | Amphipod.Desert -> D
    | _ -> failwithf "Invalid enum value"

let isOrganizedBurrow (burrow:BurrowState) = 
    if burrow.ActiveAmphipods = [] then
        true
    else
        false

let inline computeCost distance (amphipod:Amphipod) = distance * (amphipod |> int64)
let inline isFree spot burrow = burrow |> Map.containsKey spot |> not

let dist loc1 loc2 =
    match loc1, loc2 with
    | Corridor c, Room (r,d) -> Math.Abs(r - c) + d
    | Room (r,d), Corridor c -> Math.Abs(r - c) + d
    | Room (r1,d1), Room (r2,d2) when r1 = r2 -> failwithf "No reason to check Room X <-> Room X"
    | Room (r1,d1), Room (r2,d2) -> failwithf "No reason to check Room X <-> Room Y"
    | Corridor ca, Corridor cb -> failwithf "No reason to check Corridor <-> Corridor"

assert(dist (corridor 0) (room B 2) = 6)
assert(dist (room B 2) (corridor 0) = 6)
assert(dist (corridor 1) (room B 2) = 5)
assert(dist (room B 2) (corridor 1) = 5)
assert(dist (corridor 3) (room B 2) = 3)
assert(dist (room B 2) (corridor 3) = 3)
assert(dist (corridor 10) (room B 2) = 8)
assert(dist (room B 2) (corridor 10) = 8)
assert(dist (corridor 9) (room B 2) = 7)
assert(dist (room B 2) (corridor 9) = 7)

let absDist loc1 loc2 =
    match loc1, loc2 with
    | Corridor c, Room (r,d) -> Math.Abs(r - c) + d
    | Room (r,d), Corridor c -> Math.Abs(r - c) + d
    | Room (r1,d1), Room (r2,d2) when r1 = r2 -> Math.Abs (d2 - d1)
    | Room (r1,d1), Room (r2,d2) -> d1 + d2 + Math.Abs (r1 - r2)
    | Corridor ca, Corridor cb -> failwithf "No reason to check Corridor <-> Corridor"

assert(absDist (room B 2) (room C 1) = 5)
assert(absDist (room B 1) (room B 2) = 1)
       
module Part1 = 

    let isInItsPlace loc amphipod (locations:Map<Loc, Amphipod>) =
        let assigned = getAssignedRoom amphipod
        match loc with
        | Room (r, 2) when assigned = r -> true
        | Room (r, 1) when assigned = r && locations.TryFind (room r 2) |> isColorOpt amphipod -> true
        | _ -> false
    
    let getTarget amphipod (locations:Map<Loc, Amphipod>) =
        let r = getAssignedRoom amphipod
        if locations.TryFind (room r 2) |> isColorOpt amphipod then 
            r, 1 
        else 
            r, 2

    assert(getTarget Amphipod.Amber Map.empty = (A,2))
    assert(getTarget Amphipod.Bronze Map.empty = (B,2))
    assert(getTarget Amphipod.Copper Map.empty = (C,2))
    assert(getTarget Amphipod.Desert Map.empty = (D,2))
    assert(getTarget Amphipod.Amber ([(room A 2), Amphipod.Amber] |> Map.ofSeq) = (A,1))

    let getInput fileName = 
        let lines = getInputPath fileName |> File.ReadAllLines 
        let map = 
            [
                (room A 1, readAmphipod lines.[2].[3])
                (room A 2, readAmphipod lines.[3].[3])
                (room B 1, readAmphipod lines.[2].[5])
                (room B 2, readAmphipod lines.[3].[5])
                (room C 1, readAmphipod lines.[2].[7])
                (room C 2, readAmphipod lines.[3].[7])
                (room D 1, readAmphipod lines.[2].[9])
                (room D 2, readAmphipod lines.[3].[9])
            ] |> Map.ofSeq
        let actAmphipods = 
            map 
            |> Map.toSeq 
            |> Seq.filter (fun (l, a) -> isInItsPlace l a map |> not)
            |> Seq.map swap2 
            |> Seq.toList
        { Locations = map; ActiveAmphipods = actAmphipods }

    let heuristic (burrow: BurrowState) =
        // Compute theorical distance if there were no other amphipods on the grid
        burrow.ActiveAmphipods 
        |> List.map (fun (amphipod, loc) ->
            let t = getAssignedRoom amphipod
            let d = absDist (room t 2) loc 
            assert (d >= 0) 
            amphipod, d
            )
        |> List.groupBy fst
        |> List.map (fun (amphipodType, dists) -> 
            match dists with
            | [_,d] -> 
                // The remaining amphipod goes to the front of the room (hence d - 1)
                assert(d > 1)
                computeCost ((d |> int64) - 1L) amphipodType
            | [_,d1; _,d2] ->
                // When there are still 2 amphipods of the same type, use the closest one to
                // go to the back of the room in order to minimize the cost
                let minDist = min d1 d2 |> int64
                let maxDist = max d1 d2 |> int64
                assert(maxDist > 0)
                computeCost minDist amphipodType + computeCost (maxDist - 1L) amphipodType
            | _ -> 
                failwithf "Shoud have either one or two amphipods of a given type, got %i" dists.Length
        )
        |> Seq.sum
    
    let findFreeSpotInRoom r typ locs =
        match locs |> Map.tryFind (room r 1), locs |> Map.tryFind (room r 2) with
        | None, None -> (r, 2) |> Some 
        | None, Some a when a = typ -> (r,1) |> Some 
        | _ -> None

    let findFreeRoom c target typ locs =
        // TODO: get rid of the mutables ?
        let mutable blocked = false
        let mutable x = c
        let mutable result = None
        if c < target then
            while (not blocked && x < target) do
                x <- x + 1
                if x % 2 = 1 && locs |> isFree (corridor x) |> not then
                    blocked <- true
                if x = target then
                    result <- findFreeSpotInRoom x typ locs
        else if c > target then
            while (not blocked && x > target) do
                x <- x - 1
                if x % 2 = 1 && locs |> isFree (corridor x) |> not then
                    blocked <- true
                if x = target then
                    result <- findFreeSpotInRoom x typ locs
        else 
            failwithf "Invariant failure : target at same coords as corridor spot"
        result
    
    let findFreeCorridorsRight roomPos locs = seq {
        let mutable blocked = false
        let mutable x = roomPos
        while (not blocked && x < Right_back) do
            x <- x + 1
            if x % 2 = 1 then // Corridor
                if locs |> isFree (corridor x) |> not then
                    blocked <- true
                else 
                    yield corridor x
    }
    
    let findFreeCorridorsLeft roomPos locs = seq {
        let mutable blocked = false
        let mutable x = roomPos
        while (not blocked && x > Left_back) do
            x <- x - 1
            if x % 2 = 1 then // Corridor
                if locs |> isFree (corridor x) |> not then
                    blocked <- true
                else 
                    yield corridor x
    }
        
    let getUnoccupiedAvailable spot amphipod burrow = seq {
        match spot with
        | Corridor c ->
            let target, _ = getTarget amphipod burrow.Locations
            // Is the target room available ?
            match findFreeRoom c target amphipod burrow.Locations with
            | Some (r,d) ->
                let roomSpot = room r d
                yield roomSpot, dist roomSpot spot |> int64
            | None -> ()
        | Room (r, _) ->
            // Room to corridor
            yield! findFreeCorridorsRight r burrow.Locations |> Seq.map (fun loc -> loc, dist spot loc |> int64)
            yield! findFreeCorridorsLeft r burrow.Locations |> Seq.map (fun loc -> loc, dist spot loc |> int64)
    }

    let getNextStates (burrow:BurrowState) =
        burrow.ActiveAmphipods
        |> Seq.collect (fun (amphipod, oldLoc) ->
            getUnoccupiedAvailable oldLoc amphipod burrow
            |> Seq.map (fun (newLoc, distance) ->
                let nextMap = burrow.Locations |> Map.remove oldLoc |> Map.add newLoc amphipod
            
                // If the new location is the target location, update the active / inactive list
                let nextState = 
                    if isInItsPlace newLoc amphipod burrow.Locations then
                        { 
                            burrow with 
                                Locations = nextMap
                                ActiveAmphipods = burrow.ActiveAmphipods |> List.except [amphipod, oldLoc]
                        }
                    else
                        { 
                            burrow with 
                                Locations = nextMap
                                ActiveAmphipods = (amphipod, newLoc)::(burrow.ActiveAmphipods |> List.except [amphipod, oldLoc])
                        }
            
                (nextState, computeCost distance amphipod)
            )
        )

    let rec pathfindingAStarRec (openNodes:AStarNode<BurrowState> list) (closedNodes:Set<BurrowState>) =
        match openNodes with
        | [] -> 
            failwithf "No path found"
        | _ ->
            let bestNode = openNodes |> List.minBy (fun n -> n.F)
            // printfn "Selecting state %A" bestNode.Data
            //let sw = Stopwatch.StartNew()
            let isFinalState = isOrganizedBurrow bestNode.Data
            //printfn "Final state checking took %A" sw.Elapsed
            if isFinalState then
                printfn "Found optimal path with cost %i" bestNode.G
                bestNode.G
            else
                let closed = Set.add bestNode.Data closedNodes 
                let remainingOpenNodes = openNodes |> List.filter (fun n -> n <> bestNode)
    
                //sw.Restart()
                let adjacentNonClosed = 
                    getNextStates bestNode.Data 
                    |> Seq.filter (fun (newNode,_) -> closedNodes |> Set.contains newNode |> not)
                //printfn "Finding next nodes took %A" sw.Elapsed
            
                // printfn "Found %i possible targets" adjacentNonClosed.Length

                let nextOpenNodes = 
                    adjacentNonClosed 
                    |> Seq.choose (fun (newNode, cost) ->
                        // Calculate total cost of new node
                        let g = bestNode.G + cost

                        // Heuristic cost towards final state
                        //sw.Restart()
                        let h = heuristic newNode
                        //printfn "Heuristic took %A" sw.Elapsed
                        if h < 0 then
                            failwithf "Invariant failure : heuristic is negative. %i" h
                        // printfn "Heuristic %i" h
                    
                        // If this state already exists with a lower cost, skip it
                        match openNodes |> Seq.tryFind (fun n -> n.Data = newNode && g > n.G) with
                        | Some _ -> None
                        | _ -> Some { Data = newNode; F = h + g; G = g }
                    )
                
                let nextOpen = List.concat [nextOpenNodes |> Seq.toList; remainingOpenNodes]
                pathfindingAStarRec nextOpen closed 

//module Part2 = 

//    // Possible state representations :
//    // mutable array starting with 4xRoomSize then Corridor
//    // 2 mutable arrays for rooms and corridor

//    type RoomState = {
//        Position : int
//        Type : Amphipod
//        Depth: int
//        Inhabitants : Stack<Amphipod>
//        mutable CanReceive : bool
//        mutable IsDone: bool
//    } with
//        member this.AddInhabitant(a:Amphipod) =
//            if a <> this.Type then
//                failwithf "Invalid amphipod for room"
//            if this.CanReceive then
//                this.Inhabitants.Push(a)
//            if this.Inhabitants.Count = this.Depth then
//                this.IsDone <- true
//        member this.AddInitialInhabitant(a:Amphipod) =
//            if a <> this.Type then
//                this.CanReceive <- false
//            this.Inhabitants.Push(a)
//            if this.CanReceive && this.Depth = this.Inhabitants.Count then
//                this.IsDone <- true
//            this
//        member this.MoveOut() =
//            let a = this.Inhabitants.Pop()
//            if this.Inhabitants |> Seq.forall (fun i -> i = this.Type) then
//                this.CanReceive <- true
//            a
//        member this.Peek() =
//            this.Inhabitants.Peek()
//        member this.CostToEnter = this.Depth - this.Inhabitants.Count

//    type BurrowArrayState = {
//        Rooms : List<RoomState>
//        Corridor: List<(Amphipod option)>
//    }

//    let getInput fileName = 
//        let lines = getInputPath fileName |> File.ReadAllLines 
        
//        let rA = {
//            Position = 2
//            Type = Amphipod.Amber
//            Depth = 4
//            Inhabitants = new Stack<Amphipod>()
//            CanReceive = true
//            IsDone = false
//        }
//        let filledRoomA = 
//            rA
//                .AddInitialInhabitant(readAmphipod lines.[3].[3])
//                .AddInitialInhabitant(Amphipod.Desert)
//                .AddInitialInhabitant(Amphipod.Desert)
//                .AddInitialInhabitant(readAmphipod lines.[2].[3])
            
//        let rB = {
//            Position = 4
//            Type = Amphipod.Bronze
//            Depth = 4
//            Inhabitants = new Stack<Amphipod>()
//            CanReceive = true
//            IsDone = false
//        }
//        let filledRoomB = 
//            rB
//                .AddInitialInhabitant(readAmphipod lines.[3].[5])
//                .AddInitialInhabitant(Amphipod.Bronze)
//                .AddInitialInhabitant(Amphipod.Copper)
//                .AddInitialInhabitant(readAmphipod lines.[2].[5])
//        let rC = {
//            Position = 6
//            Type = Amphipod.Copper
//            Depth = 4
//            Inhabitants = new Stack<Amphipod>()
//            CanReceive = true
//            IsDone = false
//        }
//        let filledRoomC = 
//            rC
//                .AddInitialInhabitant(readAmphipod lines.[3].[7])
//                .AddInitialInhabitant(Amphipod.Amber)
//                .AddInitialInhabitant(Amphipod.Bronze)
//                .AddInitialInhabitant(readAmphipod lines.[2].[7])
//        let rD = {
//            Position = 8
//            Type = Amphipod.Desert
//            Depth = 4
//            Inhabitants = new Stack<Amphipod>()
//            CanReceive = true
//            IsDone = false
//        }
//        let filledRoomD = 
//            rD
//                .AddInitialInhabitant(readAmphipod lines.[3].[9])
//                .AddInitialInhabitant(Amphipod.Copper)
//                .AddInitialInhabitant(Amphipod.Amber)
//                .AddInitialInhabitant(readAmphipod lines.[2].[9])
//        { 
//            Rooms = new List<RoomState>([filledRoomA; filledRoomB; filledRoomC; filledRoomD])
//            Corridor = new List<Amphipod option>([None;None;None;None;None;None;None;None;None;None])
//        }

//    let heuristic (burrow: BurrowState) =
//        // Compute theorical distance if there were no other amphipods on the grid
//        burrow.ActiveAmphipods 
//        |> List.map (fun (amphipod, loc) ->
//            let t = getAssignedRoom amphipod
//            let d = absDist (room t 2) loc 
//            assert (d >= 0) 
//            amphipod, d
//            )
//        |> List.groupBy fst
//        |> List.collect (fun (amphipodType, dists) -> 
//            dists 
//            |> Seq.sortBy snd
//            |> Seq.mapi (fun i (_,d) -> computeCost ((d - i + 1) |> int64) amphipodType)
//            |> Seq.toList
//        )
//        |> Seq.sum
    
//    let findFreeSpotInRoom r typ locs =
//        let m1 = locs |> Map.tryFind (room r 1)
//        let m2 = locs |> Map.tryFind (room r 2)
//        let m3 = locs |> Map.tryFind (room r 3)
//        let m4 = locs |> Map.tryFind (room r 4)
//        match m1, m2, m3, m4 with
//        | None, None, None, None -> (r, 4) |> Some 
//        | None, None, None, Some a when a = typ -> (r, 3) |> Some 
//        | None, None, Some a, Some b when a = typ && b = typ -> (r, 2) |> Some 
//        | None, Some a, Some b, Some c when a = typ && b = typ && c = typ -> (r, 1) |> Some 
//        | _ -> None

//    let findFreeRoom c target typ locs =
//        // TODO: get rid of the mutables ?
//        let mutable blocked = false
//        let mutable x = c
//        let mutable result = None
//        if c < target then
//            while (not blocked && x < target) do
//                x <- x + 1
//                if x % 2 = 1 && locs |> isFree (corridor x) |> not then
//                    blocked <- true
//                if x = target then
//                    result <- findFreeSpotInRoom x typ locs
//        else if c > target then
//            while (not blocked && x > target) do
//                x <- x - 1
//                if x % 2 = 1 && locs |> isFree (corridor x) |> not then
//                    blocked <- true
//                if x = target then
//                    result <- findFreeSpotInRoom x typ locs
//        else 
//            failwithf "Invariant failure : target at same coords as corridor spot"
//        result
    
//    let findFreeCorridorsRight roomPos locs = seq {
//        let mutable blocked = false
//        let mutable x = roomPos
//        while (not blocked && x < Right_back) do
//            x <- x + 1
//            if x % 2 = 1 then // Corridor
//                if locs |> isFree (corridor x) |> not then
//                    blocked <- true
//                else 
//                    yield corridor x
//    }
    
//    let findFreeCorridorsLeft roomPos locs = seq {
//        let mutable blocked = false
//        let mutable x = roomPos
//        while (not blocked && x > Left_back) do
//            x <- x - 1
//            if x % 2 = 1 then // Corridor
//                if locs |> isFree (corridor x) |> not then
//                    blocked <- true
//                else 
//                    yield corridor x
//    }
        
//    let getUnoccupiedAvailable spot amphipod burrow = seq {
//        match spot with
//        | Corridor c ->
//            let target, _ = getTarget amphipod burrow.Locations
//            // Is the target room available ?
//            match findFreeRoom c target amphipod burrow.Locations with
//            | Some (r,d) ->
//                let roomSpot = room r d
//                yield roomSpot, dist roomSpot spot |> int64
//            | None -> ()
//        | Room (r, _) ->
//            // Room to corridor
//            yield! findFreeCorridorsRight r burrow.Locations |> Seq.map (fun loc -> loc, dist spot loc |> int64)
//            yield! findFreeCorridorsLeft r burrow.Locations |> Seq.map (fun loc -> loc, dist spot loc |> int64)
//    }

//    let getNextStates (burrow:BurrowState) =
//        burrow.ActiveAmphipods
//        |> Seq.collect (fun (amphipod, oldLoc) ->
//            getUnoccupiedAvailable oldLoc amphipod burrow
//            |> Seq.map (fun (newLoc, distance) ->
//                let nextMap = burrow.Locations |> Map.remove oldLoc |> Map.add newLoc amphipod
            
//                // If the new location is the target location, update the active / inactive list
//                let nextState = 
//                    if isInItsPlace newLoc amphipod burrow.Locations then
//                        { 
//                            burrow with 
//                                Locations = nextMap
//                                ActiveAmphipods = burrow.ActiveAmphipods |> List.except [amphipod, oldLoc]
//                        }
//                    else
//                        { 
//                            burrow with 
//                                Locations = nextMap
//                                ActiveAmphipods = (amphipod, newLoc)::(burrow.ActiveAmphipods |> List.except [amphipod, oldLoc])
//                        }
            
//                (nextState, computeCost distance amphipod)
//            )
//        )

//    let mutable openCount = 0
//    let mutable closedCount = 0
//    let mutable iterations = 0
//    let rec pathfindingAStarRec (openNodes:AStarNode<BurrowState> list) (closedNodes:Set<BurrowState>) =
//        openCount <- openNodes.Length
//        iterations <- iterations + 1
//        if iterations % 100 = 0 then 
//            printfn "Iteration %i. Open %i. Closed %i" iterations openCount closedCount
//        match openNodes with
//        | [] -> 
//            failwithf "No path found"
//        | _ ->
//            let bestNode = openNodes |> List.minBy (fun n -> n.F)
//            // printfn "Selecting state %A" bestNode.Data
//            //let sw = Stopwatch.StartNew()
//            let isFinalState = isOrganizedBurrow bestNode.Data
//            //printfn "Final state checking took %A" sw.Elapsed
//            if isFinalState then
//                printfn "Found optimal path with cost %i" bestNode.G
//                bestNode.G
//            else
//                let closed = Set.add bestNode.Data closedNodes 
//                closedCount <- closedCount + 1
//                let remainingOpenNodes = openNodes |> List.filter (fun n -> n <> bestNode)
    
//                //sw.Restart()
//                let adjacentNonClosed = 
//                    getNextStates bestNode.Data 
//                    |> Seq.filter (fun (newNode,_) -> closedNodes |> Set.contains newNode |> not)
//                //printfn "Finding next nodes took %A" sw.Elapsed
            
//                // printfn "Found %i possible targets" adjacentNonClosed.Length

//                let nextOpenNodes = 
//                    adjacentNonClosed 
//                    |> Seq.choose (fun (newNode, cost) ->
//                        // Calculate total cost of new node
//                        let g = bestNode.G + cost

//                        // Heuristic cost towards final state
//                        //sw.Restart()
//                        let h = heuristic newNode
//                        //printfn "Heuristic took %A" sw.Elapsed
//                        if h < 0 then
//                            failwithf "Invariant failure : heuristic is negative. %i" h
//                        //printfn "Heuristic %i" h
                    
//                        // If this state already exists with a lower cost, skip it
//                        match openNodes |> Seq.tryFind (fun n -> n.Data = newNode && g > n.G) with
//                        | Some _ -> None
//                        | _ -> Some { Data = newNode; F = h + g; G = g }
//                    )
                
//                let nextOpen = List.concat [nextOpenNodes |> Seq.toList; remainingOpenNodes]
//                pathfindingAStarRec nextOpen closed 


//assert(Part1.pathfindingAStarRec [{ Data = Part1.getInput "Day23_sample2.txt"; G = 0; F = 0 }] Set.empty = 46L)

//let sw = Stopwatch.StartNew()
//printfn "Starting"
//let result = Part1.pathfindingAStarRec [{ Data = Part1.getInput "Day23.txt"; G = 0; F = 0 }] Set.empty
//printfn "Part 1 solution : %i. Found in %A" result sw.Elapsed

//let expected = 44169
Part1.pathfindingAStarRec [{ Data = Part1.getInput "Day23_sample1.txt"; G = 0; F = 0 }] Set.empty

