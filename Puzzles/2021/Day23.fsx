#load "../../Tools.fsx"

open System
open System.Diagnostics
open System.IO
open Tools
open Tools.AStar

type AmphipodType = | Amber = 1 | Bronze = 10 | Copper = 100  | Desert = 1000
type Amphipod = { Type: AmphipodType(*; Id: int*) }

// let mutable id = 0;
let readAmphipod c =
    // id <- id + 1
    match c with
    | 'A' -> { Type = AmphipodType.Amber (*; Id = id*) }
    | 'B' -> { Type = AmphipodType.Bronze(*; Id = id*) }
    | 'C' -> { Type = AmphipodType.Copper(*; Id = id*) }
    | 'D' -> { Type = AmphipodType.Desert(*; Id = id*) }
    | _ -> failwithf "Invalid amphipod type"

let Left_back = 0
let Left_front = 1
let A = 2 
let A_B = 3
let B = 4
let B_C = 5
let C = 6
let C_D = 7
let D = 8
let Right_front = 9
let Right_back = 10

type Loc =
| Corridor of int
| Room of int * int

type BurrowState = {
    Locations : Map<Loc, Amphipod>
    ActiveAmphipods : (Amphipod * Loc) list
    InactiveAmphipods : (Amphipod * Loc) list
}

let inline room r d = 
    if d = 1 || d = 2 then 
        if r = A || r = B || r = C || r = D then
            Room (r, d) 
        else
            failwithf "Invalid room %i" r
    else 
        failwithf "Invalid depth %i" d

// let inline room2 (r,d) = room r d

let inline corridor c =
    if c >= Left_back && c <= Right_back && c <> A && c <> B && c <> C && c <> D then
        Corridor c
    else
        failwithf "Invalid corridor %i" c

// let inline isColor type' amphipod = amphipod.Type = type'
let inline isColorOpt type' = 
    function 
    | Some a when a.Type = type' -> true 
    // | Some _ -> failwithf "Trying to find a target to a room occupied by another type of ampthipod"
    | _ -> false
    
let getAssignedRoom = 
    function 
    | AmphipodType.Amber -> A 
    | AmphipodType.Bronze -> B 
    | AmphipodType.Copper -> C
    | AmphipodType.Desert -> D
    | _ -> failwithf "Invalid enum value"
        
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

assert(getTarget AmphipodType.Amber Map.empty = (A,2))
assert(getTarget AmphipodType.Bronze Map.empty = (B,2))
assert(getTarget AmphipodType.Copper Map.empty = (C,2))
assert(getTarget AmphipodType.Desert Map.empty = (D,2))
assert(getTarget AmphipodType.Amber ([(room A 2), { Type = AmphipodType.Amber(*; Id = 0*)}] |> Map.ofSeq) = (A,1))

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
        |> Seq.filter (fun (l, a) -> isInItsPlace l a.Type map |> not)
        |> Seq.map swap2 
        |> Seq.toList
    let inactiveAmphipods = 
        map 
        |> Map.toSeq 
        |> Seq.filter (fun (l, a) -> isInItsPlace l a.Type map)
        |> Seq.map swap2 
        |> Seq.toList
    { Locations = map; ActiveAmphipods = actAmphipods; InactiveAmphipods = inactiveAmphipods }

let isOrganizedBurrow (burrow:BurrowState) = 
    if burrow.ActiveAmphipods = [] then
        // TODO: all amphipods must be in their room
        if burrow.InactiveAmphipods.Length = 8 then
            true
        else
            failwithf "Invariant check failed. Active = 0 but Inactive = %i" burrow.InactiveAmphipods.Length
    else
        false

let inline computeCost distance (amphipod:AmphipodType) = distance * (amphipod |> int64)
let inline isFree spot burrow = burrow |> Map.containsKey spot |> not
// let inline areFree spots burrow = spots |> Seq.forall (fun s -> isFree s burrow)

let dist loc1 loc2 =
    match loc1, loc2 with
    | Corridor ca, Corridor cb -> failwithf "No reason to check Corridor <-> Corridor" //Math.Abs(cb - ca)
    | Corridor c, Room (r,d) -> Math.Abs(r - c) + d
    | Room (r,d), Corridor c -> Math.Abs(r - c) + d
    | Room (r1,d1), Room (r2,d2) when r1 = r2 -> failwithf "No reason to check Room X <-> Room X" //Math.Abs (d2 - d1)
    | Room (r1,d1), Room (r2,d2) -> failwithf "No reason to check Room X <-> Room Y" // d1 + d2 + Math.Abs (r1 - r2)

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
    | Corridor ca, Corridor cb -> failwithf "No reason to check Corridor <-> Corridor" // Math.Abs(cb - ca)
    | Corridor c, Room (r,d) -> Math.Abs(r - c) + d
    | Room (r,d), Corridor c -> Math.Abs(r - c) + d
    | Room (r1,d1), Room (r2,d2) when r1 = r2 -> Math.Abs (d2 - d1)
    | Room (r1,d1), Room (r2,d2) -> d1 + d2 + Math.Abs (r1 - r2)

assert(absDist (room B 2) (room C 1) = 5)
assert(absDist (room B 1) (room B 2) = 1)

let heuristic (burrow: BurrowState) =
    // Compute theorical distance if there were no other amphipods on the grid
    burrow.ActiveAmphipods 
    |> List.map (fun (amphipod, loc) ->
        let t = getAssignedRoom amphipod.Type
        let d = absDist (room t 2) loc 
        assert (d >= 0) 
        amphipod.Type, d
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
    | None, Some a when a.Type = typ -> (r,1) |> Some 
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
        let target, _ = getTarget amphipod.Type burrow.Locations
        // Is the target room available ?
        match findFreeRoom c target amphipod.Type burrow.Locations with
        | Some (r,d) ->
            let roomSpot = room r d
            yield roomSpot, dist roomSpot spot |> int64
        | None -> ()
    | Room (r, _) ->
        // Room to corridor
        yield! findFreeCorridorsRight r burrow.Locations |> Seq.map (fun loc -> loc, dist spot loc |> int64)
        yield! findFreeCorridorsLeft r burrow.Locations |> Seq.map (fun loc -> loc, dist spot loc |> int64)
}

let getNextStates (burrow:BurrowState) : (BurrowState * int64) list =
    burrow.ActiveAmphipods
    |> Seq.collect (fun (amphipod, oldLoc) ->
        getUnoccupiedAvailable oldLoc amphipod burrow
        |> Seq.map (fun (newLoc, distance) ->
            let nextMap = burrow.Locations |> Map.remove oldLoc |> Map.add newLoc amphipod
            
            // If the new location is the target location, update the active / inactive list
            let nextState = 
                if isInItsPlace newLoc amphipod.Type burrow.Locations then
                    { 
                        burrow with 
                            Locations = nextMap
                            ActiveAmphipods = burrow.ActiveAmphipods |> List.except [amphipod, oldLoc]
                            InactiveAmphipods = (amphipod, newLoc)::burrow.InactiveAmphipods
                    }
                else
                    { 
                        burrow with 
                            Locations = nextMap
                            ActiveAmphipods = (amphipod, newLoc)::(burrow.ActiveAmphipods |> List.except [amphipod, oldLoc])
                    }
            
            (nextState, computeCost distance amphipod.Type)
        )
    )
    |> Seq.toList

let rec pathfindingAStarRec (openNodes:AStarNode<BurrowState> list) (closedNodes:Set<BurrowState>) =
    match openNodes with
    | [] -> failwithf "No path found"
    | _ -> 
        let bestNode = openNodes |> Seq.minBy (fun n -> n.F)
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
                |> List.filter (fun (newNode,_) -> closedNodes |> Set.contains newNode |> not)
            //printfn "Finding next nodes took %A" sw.Elapsed
            
            // printfn "Found %i possible targets" adjacentNonClosed.Length

            let nextOpenNodes = 
                adjacentNonClosed 
                |> List.choose (fun (newNode, cost) ->
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
                    match openNodes |> List.tryFind (fun n -> n.Data = newNode && g > n.G) with
                    | Some _ -> None
                    | _ -> Some { Data = newNode; F = h + g; G = g }
                )
                
            let nextOpen = List.concat [nextOpenNodes; remainingOpenNodes]
            pathfindingAStarRec nextOpen closed 

//let startSituation = 
assert(pathfindingAStarRec [{ Data = getInput "Day23_sample2.txt"; G = 0; F = 0 }] Set.empty = 46L)

let sw = Stopwatch.StartNew()
let result = pathfindingAStarRec [{ Data = getInput "Day23.txt"; G = 0; F = 0 }] Set.empty
printfn "Part 1 solution : %i. Found in %A" result sw.Elapsed