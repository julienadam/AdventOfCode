#load "../../Tools.fsx"

open System
open System.Collections.Generic
open System.Diagnostics
open System.IO
open System.Text.RegularExpressions
open Tools

type AmphipodType = | Amber = 1 | Bronze = 10 | Copper = 100  | Desert = 1000

let readAmphipod =
    function
    | 'A' -> AmphipodType.Amber
    | 'B' -> AmphipodType.Bronze
    | 'C' -> AmphipodType.Copper
    | 'D' -> AmphipodType.Desert
    | _ -> failwithf "Invalid amphipod type"
    
type Spots =
    | Corridor_left_back = 0
    | Corridor_left_front = 1
    | RoomA_back = 2
    | RoomA_front = 3
    | Corridor_A_B = 4
    | RoomB_back = 5
    | RoomB_front = 6
    | Corridor_B_C = 7
    | RoomC_back = 8
    | RoomC_front = 9
    | Corridor_C_D = 10
    | RoomD_back = 11
    | RoomD_front = 12
    | Corridor_right_front = 13
    | Corridor_right_back = 14

type BurrowState = Map<Spots, AmphipodType> // TODO some sort of builder to check state at creation time

// Returns x,y coords for a spot
let spotPos = 
    function
    | Spots.Corridor_left_back    -> (0,0)
    | Spots.Corridor_left_front   -> (0,1)
    | Spots.RoomA_back            -> (2,2)
    | Spots.RoomA_front           -> (2,1)
    | Spots.Corridor_A_B          -> (3,0)
    | Spots.RoomB_back            -> (4,2)
    | Spots.RoomB_front           -> (4,1)
    | Spots.Corridor_B_C          -> (5,0)
    | Spots.RoomC_back            -> (6,2)
    | Spots.RoomC_front           -> (6,1)
    | Spots.Corridor_C_D          -> (7,0)
    | Spots.RoomD_back            -> (8,2)
    | Spots.RoomD_front           -> (8,1)
    | Spots.Corridor_right_front  -> (9,0)
    | Spots.Corridor_right_back   -> (10,0)
    | _ -> failwithf "Invalid spot"

let getInput fileName = 
    let lines = getInputPath fileName |> File.ReadAllLines 
    [
        (Spots.RoomA_front, readAmphipod lines.[2].[3])
        (Spots.RoomA_back, readAmphipod lines.[3].[3])
        (Spots.RoomB_front, readAmphipod lines.[2].[5])
        (Spots.RoomB_back, readAmphipod lines.[3].[5])
        (Spots.RoomC_front, readAmphipod lines.[2].[7])
        (Spots.RoomC_back, readAmphipod lines.[3].[7])
        (Spots.RoomD_front, readAmphipod lines.[2].[9])
        (Spots.RoomD_back, readAmphipod lines.[3].[9])
    ] |> Map.ofSeq


let isOrganizedBurrow (burrow:BurrowState) =
       burrow.TryFind Spots.RoomA_back  = Some AmphipodType.Amber
    && burrow.TryFind Spots.RoomA_front = Some AmphipodType.Amber
    && burrow.TryFind Spots.RoomB_back  = Some AmphipodType.Bronze
    && burrow.TryFind Spots.RoomB_front = Some AmphipodType.Bronze
    && burrow.TryFind Spots.RoomC_back  = Some AmphipodType.Copper
    && burrow.TryFind Spots.RoomC_front = Some AmphipodType.Copper
    && burrow.TryFind Spots.RoomD_back  = Some AmphipodType.Desert
    && burrow.TryFind Spots.RoomD_front = Some AmphipodType.Desert

type AStarNode<'a> = {
    F: int64
    G: int64
    Data: 'a
}

//let getAvailableTargetSpots (spot:Spots) =
//    match spot with
//    | Spots.Corridor_left_back   -> [(Spots.Corridor_left_front, 1)]
//    | Spots.Corridor_left_front  -> [(Spots.RoomA_front, 2); (Spots.Corridor_A_B, 2); (Spots.Corridor_left_back, 1) ]
//    | Spots.RoomA_front          -> [(Spots.RoomA_back,1); (Spots.Corridor_left_front,2); (Spots.Corridor_A_B,2) ]
//    | Spots.RoomA_back           -> [(Spots.RoomA_front, 1)]
//    | Spots.Corridor_A_B         -> [(Spots.Corridor_left_front, 2); (Spots.RoomA_front, 2); (Spots.RoomB_front, 2); (Spots.Corridor_B_C, 2) ]
//    | Spots.RoomB_front          -> [(Spots.Corridor_A_B, 2); (Spots.Corridor_B_C, 2); (Spots.RoomB_back, 1)]
//    | Spots.RoomB_back           -> [(Spots.RoomB_front, 1)]
//    | Spots.Corridor_B_C         -> [(Spots.RoomC_front, 2); (Spots.RoomB_front, 2); (Spots.Corridor_A_B, 2); (Spots.Corridor_C_D, 2) ]
//    | Spots.RoomC_front          -> [(Spots.Corridor_C_D, 2); (Spots.Corridor_B_C, 2); (Spots.RoomC_back, 1) ]
//    | Spots.RoomC_back           -> [(Spots.RoomC_front, 1)]
//    | Spots.Corridor_C_D         -> [(Spots.Corridor_B_C, 2); (Spots.RoomC_front, 2); (Spots.RoomD_front, 2); (Spots.Corridor_right_front, 2)]
//    | Spots.RoomD_front          -> [(Spots.Corridor_C_D, 2); (Spots.Corridor_right_front, 2); (Spots.RoomD_back, 1) ]
//    | Spots.RoomD_back           -> [(Spots.RoomD_front, 1)]
//    | Spots.Corridor_right_front -> [(Spots.Corridor_right_back, 1); (Spots.RoomD_front, 2); (Spots.Corridor_C_D, 2)]
//    | Spots.Corridor_right_back  -> [(Spots.Corridor_right_front, 1)]
//    | _ -> failwithf "No reason for this. Invalid enum value"

//let getUnoccupiedAvailable spot (burrow:BurrowState) =
//    getAvailableTargetSpots spot |> List.filter (fun (s,_) -> burrow |> Map.containsKey s |> not)

let computeCost distance (amphipod:AmphipodType) =
    distance * (amphipod |> int64)

let getTarget amphipod (burrow:BurrowState) =
    match amphipod with
    | AmphipodType.Amber  -> if burrow.TryFind Spots.RoomA_back = Some AmphipodType.Amber then Spots.RoomA_front else Spots.RoomA_back
    | AmphipodType.Bronze -> if burrow.TryFind Spots.RoomB_back = Some AmphipodType.Bronze then Spots.RoomB_front else Spots.RoomB_back
    | AmphipodType.Copper -> if burrow.TryFind Spots.RoomC_back = Some AmphipodType.Copper then Spots.RoomC_front else Spots.RoomC_back
    | AmphipodType.Desert -> if burrow.TryFind Spots.RoomD_back = Some AmphipodType.Desert then Spots.RoomC_front else Spots.RoomD_back
    | _ -> failwithf "Invalid amphipod, spot combination"

let getUnoccupiedAvailable spot amphipod burrow = seq {
    match spot |> spotPos with
    | x, 0 -> 
        // Corridor -> rooms
        match amphipod with
        | AmphipodType.Amber ->
            let target = getTarget amphipod burrow
            let tx,ty = target |> spotPos


        //match x, amphipod with
        //| 0, AmphipodType.Amber ->
        //    if burrow |> Map.containsKey Spots.Corridor_left_front |> not then
        //        if burrow |> Map.containsKey Spots.RoomA_front |> not then
                    
            //if burrow |> Map.containsKey Spots.Corridor_left_front |> not then
            //    yield Spots.Corridor_left_front
            //    yield Spots.Corridor_left_front
        // Corridor -> final room
        yield! []
    | x, 1 -> 
        // Front of room -> corridor
        yield! []
    | x, 2 -> 
        // Back of room -> corridor
        // Check corridor spots
        yield! []
    | _ -> failwithf "Invalid position"
    }

let getNextStates (burrow:BurrowState) : (BurrowState * int64) list =
    burrow
    |> Map.toSeq
    |> Seq.filter (fun (spot, amphipod) -> spot <> getTarget amphipod burrow) // These are already in the right spot
    |> Seq.collect (fun (spot, amphipod) ->
        getUnoccupiedAvailable spot amphipod burrow
        |> Seq.map (fun (s, distance) ->
            let nextState:BurrowState = burrow |> Map.remove spot |> Map.add s amphipod
            let nextCost = computeCost distance amphipod
            (nextState, nextCost)
        )
    )
    |> Seq.toList

(*
#01234567890#
#...........#
###B#C#B#D###
  #A#D#C#A#  
  #########  
*)

let getAbsoluteTarget amphipod =
    match amphipod with
    | AmphipodType.Amber  -> Spots.RoomA_back
    | AmphipodType.Bronze -> Spots.RoomB_back
    | AmphipodType.Copper -> Spots.RoomC_back
    | AmphipodType.Desert -> Spots.RoomC_back
    | _ -> failwithf "Invalid amphipod, spot combination"

let pseudoManhattan s1 s2 =
    let (x1,y1),(x2,y2) = s1 |> spotPos, s2 |> spotPos
    Math.Abs(x2-x1) + (if x1 <> x2 then y1 + y2 else Math.Abs(y2 - y1))

let heuristic (burrow:BurrowState) =
    burrow 
    |> Map.toSeq 
    |> Seq.map (fun (spot, amphipod) ->
        let t = getAbsoluteTarget amphipod
        if t = spot then 
            amphipod, 0
        else
            amphipod, pseudoManhattan t spot
    )
    |> Seq.groupBy fst
    |> Seq.map (fun (g, dists) -> 
        let min = dists |> Seq.map snd |> Seq.min |> int64
        let max = dists |> Seq.map snd |> Seq.max |> int64
        computeCost min g + computeCost (max - 1L) g
    )
    |> Seq.sum


let rec pathfindingAStarRec (openNodes:AStarNode<BurrowState> list) (closedNodes:AStarNode<BurrowState> list) =
    match openNodes with
    | [] -> failwithf "No path found"
    | _ -> 
        let q = openNodes |> Seq.minBy (fun n -> n.F)
        //printfn "Selecting state %A" q.Data
        if isOrganizedBurrow q.Data then
            printfn "Found optimal path with cost %i" q.G
        else
            let closed = q :: closedNodes
            let openn = openNodes |> List.filter (fun n -> n <> q)
    
            // TODO switch to map
            let adjacentNonClosed = 
                getNextStates q.Data |> List.filter (fun (nq,_) -> closedNodes |> List.exists (fun a -> a.Data = nq) |> not)

            //printfn "Found %i possible targets" adjacentNonClosed.Length

            let nextOpenNodes = 
                adjacentNonClosed 
                |> List.choose (fun (nq,cost) ->
                    
                    // Calculate total cost of new node
                    let g = q.G + cost

                    // Heuristic cost towards final state
                    let h = heuristic nq
                    //printfn "Heuristic %i" h

                    // If this state already exists with a lower cost, stop
                    match openNodes |> List.tryFind (fun n -> n.Data = nq && g > n.G) with
                    | Some _ -> None
                    | _ -> Some { Data = nq; F = h + g; G = g }
                )
                
            let nextOpen = List.concat [nextOpenNodes; openn]
            pathfindingAStarRec nextOpen closed 


let startSituation = getInput "Day23_sample1.txt"
pathfindingAStarRec [{ Data = startSituation; G = 0; F = 0 }] []
