#load "../../Tools.fsx"

open System
open System.Collections.Generic
open System.Diagnostics
open System.IO
open System.Text.RegularExpressions
open Tools

type AmphipodType =
    | Amber = 1
    | Bronze = 10
    | Copper = 100
    | Desert = 1000

let readAmphipod c =
    match c with 
    | 'A' -> AmphipodType.Amber
    | 'B' -> AmphipodType.Bronze
    | 'C' -> AmphipodType.Copper
    | 'D' -> AmphipodType.Desert
    | _ -> failwithf "Invalid amphipod type %c" c
    
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

let getAvailableTargetSpots (spot:Spots) =
    match spot with
    | Spots.Corridor_left_back   -> [(Spots.Corridor_left_front, 1)]
    | Spots.Corridor_left_front  -> [(Spots.RoomA_front, 2); (Spots.Corridor_A_B, 2); (Spots.Corridor_left_back, 1) ]
    | Spots.RoomA_front          -> [(Spots.RoomA_back,1); (Spots.Corridor_left_front,2); (Spots.Corridor_A_B,2) ]
    | Spots.RoomA_back           -> [(Spots.RoomA_front, 1)]
    | Spots.Corridor_A_B         -> [(Spots.Corridor_left_front, 2); (Spots.RoomA_front, 2); (Spots.RoomB_front, 2); (Spots.Corridor_B_C, 2) ]
    | Spots.RoomB_front          -> [(Spots.Corridor_A_B, 2); (Spots.Corridor_B_C, 2); (Spots.RoomB_back, 1)]
    | Spots.RoomB_back           -> [(Spots.RoomB_front, 1)]
    | Spots.Corridor_B_C         -> [(Spots.RoomC_front, 2); (Spots.RoomB_front, 2); (Spots.Corridor_A_B, 2); (Spots.Corridor_C_D, 2) ]
    | Spots.RoomC_front          -> [(Spots.Corridor_C_D, 2); (Spots.Corridor_B_C, 2); (Spots.RoomC_back, 1) ]
    | Spots.RoomC_back           -> [(Spots.RoomC_front, 1)]
    | Spots.Corridor_C_D         -> [(Spots.Corridor_B_C, 2); (Spots.RoomC_front, 2); (Spots.RoomD_front, 2); (Spots.Corridor_right_front, 2)]
    | Spots.RoomD_front          -> [(Spots.Corridor_C_D, 2); (Spots.Corridor_right_front, 2); (Spots.RoomD_back, 1) ]
    | Spots.RoomD_back           -> [(Spots.RoomD_front, 1)]
    | Spots.Corridor_right_front -> [(Spots.Corridor_right_back, 1); (Spots.RoomD_front, 2); (Spots.Corridor_C_D, 2)]
    | Spots.Corridor_right_back  -> [(Spots.Corridor_right_front, 1)]
    | _ -> failwithf "No reason for this. Invalid enum value"

let getUnoccupiedAvailable spot (burrow:BurrowState) =
    getAvailableTargetSpots spot |> List.filter (fun (s,_) -> burrow |> Map.containsKey s |> not)

let isInFinalState spot amphipod (burrow:BurrowState) =
    match amphipod,spot with
    | AmphipodType.Amber, Spots.RoomA_back -> true
    | AmphipodType.Amber, Spots.RoomA_front -> burrow.TryFind Spots.RoomA_back = Some AmphipodType.Amber
    | AmphipodType.Amber, _ -> false
    | AmphipodType.Bronze, Spots.RoomB_back -> true
    | AmphipodType.Bronze, Spots.RoomB_front -> burrow.TryFind Spots.RoomB_back = Some AmphipodType.Bronze
    | AmphipodType.Bronze, _ -> false
    | AmphipodType.Copper, Spots.RoomC_back -> true
    | AmphipodType.Copper, Spots.RoomC_front -> burrow.TryFind Spots.RoomC_back = Some AmphipodType.Copper
    | AmphipodType.Copper, _ -> false
    | AmphipodType.Desert, Spots.RoomD_back -> true
    | AmphipodType.Desert, Spots.RoomD_front -> burrow.TryFind Spots.RoomD_back = Some AmphipodType.Desert
    | AmphipodType.Desert, _ -> false
    | _ -> failwithf "Invalid amphipod, spot combination"

let computeCost distance (amphipod:AmphipodType) =
    distance * (amphipod |> int)

let evolveState (burrow:BurrowState) currentCost =
    burrow |> Seq.collect (fun kvp ->
        let (spot, amphipod) = kvp.Key,kvp.Value
        if isInFinalState spot amphipod burrow then
            []
        else
            getUnoccupiedAvailable spot burrow
            |> List.map (fun (s,distance) ->
                let nextState = burrow |> Map.remove spot |> Map.add s amphipod
                let nextCost = currentCost + computeCost distance amphipod
                (nextState, nextCost)
            )
    )

let isOrganizedBurrow (burrow:BurrowState) =
       burrow.TryFind Spots.RoomA_back  = Some AmphipodType.Amber
    && burrow.TryFind Spots.RoomA_front = Some AmphipodType.Amber
    && burrow.TryFind Spots.RoomB_back  = Some AmphipodType.Bronze
    && burrow.TryFind Spots.RoomB_front = Some AmphipodType.Bronze
    && burrow.TryFind Spots.RoomC_back  = Some AmphipodType.Copper
    && burrow.TryFind Spots.RoomC_front = Some AmphipodType.Copper
    && burrow.TryFind Spots.RoomD_back  = Some AmphipodType.Desert
    && burrow.TryFind Spots.RoomD_front = Some AmphipodType.Desert

let solve1 (burrow:BurrowState) =
    let mutable counter = 0
    let mutable openPaths = 0
    let mutable closedPaths = 0
    let mutable successfulPaths = 0
    let mutable minCost = None
    let visited = new Dictionary<BurrowState, int>()

    let rec solve (burrow:BurrowState) currentCost =
        counter <- counter + 1
        if counter % 10000 = 0 then
            printfn "Iteration %i. Open paths %i. Closed paths %i. Successful paths %i" counter openPaths closedPaths successfulPaths
        match minCost with
        | Some c when currentCost > c -> 
            closedPaths <- closedPaths + 1
            None
        | _ ->
            match visited.TryGetValue burrow with
            | true, c when currentCost > c -> 
                closedPaths <- closedPaths + 1
                None
            | _ ->
                visited.[burrow] <- currentCost
                if isOrganizedBurrow burrow then
                    if (not minCost.IsSome) || (currentCost < minCost.Value) then
                        printfn "Found new best cost %i" currentCost
                        minCost <- Some currentCost
                        successfulPaths <- successfulPaths + 1
                        Some currentCost
                    else
                        closedPaths <- closedPaths + 1
                        None
                else
                    let nextStages = evolveState burrow currentCost |> Seq.toList
                    openPaths <- openPaths + nextStages.Length
                    let solvedNextStages =
                        nextStages 
                        |> List.choose (fun (nextBurrow, nextCost) -> solve nextBurrow nextCost)
                    if solvedNextStages.Length > 0 then
                        solvedNextStages |> List.min  |> Some
                    else
                        None

    solve burrow 0

let startSituation = getInput "Day23_sample1.txt"
solve1 startSituation

// Expected : 12501

(*
Problem space :
4 rooms with a back and a front, 8 spots total
2 rooms with a back and front, one on the A side, one on the D side, 4 spots total
3 corridor spots named AB, BC, CD
8 amphipods, by pairs
            
// Puzzle representation
#############
#...........#
###B#C#B#D###
  #A#D#C#A#  
  #########  
            
// Graph representation
CA2-CA1--SAB--SBC--SCD--CD1-CD2
        \   / \  / \  / \   /
        \ /   \/   \/   \ /
        RA1  RB1   RC1  RD1
        |    |     |    |
        RA2  RB2   RC2  RD2
            
*)
