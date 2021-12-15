<Query Kind="FSharpProgram">
  <NuGetReference>FSharpPlus</NuGetReference>
  <Namespace>FSharpPlus</Namespace>
</Query>

let getInputPath file = 
    Path.Combine(
        Environment.GetFolderPath(Environment.SpecialFolder.UserProfile), 
        @"CloudStation\Data\AdventOfCode\", 
        file)
        
let path = getInputPath "day17.txt"
let parseCubeState c = match c with | '#' -> true | '.' -> false | c -> failwithf "%c is not a valid cube state" c
    
module Puzzle1 = 
        
    type Point3D = { x: int; y: int; z: int }
    
    let parseLine y l = l |> Seq.mapi (fun x c -> { x = x; y = y; z = 0 }, (parseCubeState c))
    
    let initialState =
        File.ReadAllLines(path) 
        |> Seq.mapi parseLine 
        |> Seq.collect id
        |> Map.ofSeq
    
    let getNeighboringCoordinates point = seq {
        for i = -1 to 1 do
            for j = -1 to 1 do
                for k = -1 to 1 do
                    if i <> 0 || j <> 0 || k <> 0 then
                        yield { x = point.x + i; y = point.y + j; z = point.z + k }
    }
       
    let applyRulesToCube (point, value) state =
        
        let numNeighbors =
            point 
            |> getNeighboringCoordinates
            |> Seq.map (fun p -> 
                match state |> Map.tryFind p with
                | Some true -> 1
                | _ -> 0)
            |> Seq.sum
        
        let nextVal = 
            match value, numNeighbors with
            | true, 2 -> true
            | true, 3 -> true
            | true, _ -> false
            | false, 3 -> true
            | false, _ -> false
        
        (point, nextVal)
        
    
    let getAllPointsBetween pointA pointB = seq {
        for i = pointA.x to pointB.x do
            for j = pointA.y to pointB.y do
                for k = pointA.z to pointB.z do
                    yield { x = i; y = j; z = k }
    }
        
    let applyRulesToState (state: Map<Point3D, bool>) =
        let min = 
            { 
                x = (state |> Seq.map (fun k -> k.Key.x) |> Seq.min) - 1; 
                y = (state |> Seq.map (fun k -> k.Key.y) |> Seq.min) - 1; 
                z = (state |> Seq.map (fun k -> k.Key.z) |> Seq.min) - 1; 
            }
        let max =
            { 
                x = (state |> Seq.map (fun k -> k.Key.x) |> Seq.max) + 1; 
                y = (state |> Seq.map (fun k -> k.Key.y) |> Seq.max) + 1; 
                z = (state |> Seq.map (fun k -> k.Key.z) |> Seq.max) + 1; 
            }
        
        let modifiedCubes = 
            getAllPointsBetween min max
            |> Seq.map (fun p ->
                let v = match state.TryFind p with | Some true -> true | _ -> false
                applyRulesToCube (p, v) state)
            |> Map.ofSeq
            
        // Merge the new cubes in the original state
        state |> Map.union modifiedCubes
        
    let solution() =
        [1..6]
        |> Seq.fold (fun state _ -> applyRulesToState state) initialState
        |> Seq.map (fun kv -> kv.Value)
        |> Seq.filter id
        |> Seq.length
        |> Dump
        
// Puzzle1.solution()

module Puzzle2 =
    
    type Point4D = { x: int; y: int; z: int; w: int }
    
    let parseLine y l = l |> Seq.mapi (fun x c -> { x = x; y = y; z = 0; w = 0 }, (parseCubeState c))
    
    let initialState =
        File.ReadAllLines(path) 
        |> Seq.mapi parseLine 
        |> Seq.collect id
        |> Map.ofSeq
    
    let getNeighboringCoordinates point = seq {
        for i = -1 to 1 do
            for j = -1 to 1 do
                for k = -1 to 1 do
                    for l = -1 to 1 do
                        if i <> 0 || j <> 0 || k <> 0 || l <> 0  then
                            yield { x = point.x + i; y = point.y + j; z = point.z + k; w = point.w + l }
    }
       
    let applyRulesToCube (point, value) state =
        
        let numNeighbors =
            point 
            |> getNeighboringCoordinates
            |> Seq.map (fun p -> 
                match state |> Map.tryFind p with
                | Some true -> 1
                | _ -> 0)
            |> Seq.sum
        
        let nextVal = 
            match value, numNeighbors with
            | true, 2 -> true
            | true, 3 -> true
            | true, _ -> false
            | false, 3 -> true
            | false, _ -> false
        
        (point, nextVal)
        
    
    let getAllPointsBetween pointA pointB = seq {
        for i = pointA.x to pointB.x do
            for j = pointA.y to pointB.y do
                for k = pointA.z to pointB.z do
                    for l = pointA.w to pointB.w do
                    yield { x = i; y = j; z = k; w = l }
    }
        
    let applyRulesToState (state: Map<Point4D, bool>) =
        let min = 
            { 
                x = (state |> Seq.map (fun k -> k.Key.x) |> Seq.min) - 1; 
                y = (state |> Seq.map (fun k -> k.Key.y) |> Seq.min) - 1; 
                z = (state |> Seq.map (fun k -> k.Key.z) |> Seq.min) - 1; 
                w = (state |> Seq.map (fun k -> k.Key.w) |> Seq.min) - 1; 
            }
        let max =
            { 
                x = (state |> Seq.map (fun k -> k.Key.x) |> Seq.max) + 1; 
                y = (state |> Seq.map (fun k -> k.Key.y) |> Seq.max) + 1; 
                z = (state |> Seq.map (fun k -> k.Key.z) |> Seq.max) + 1; 
                w = (state |> Seq.map (fun k -> k.Key.w) |> Seq.max) + 1; 
            }
        
        let modifiedCubes = 
            getAllPointsBetween min max
            |> Seq.map (fun p ->
                let v = match state.TryFind p with | Some true -> true | _ -> false
                applyRulesToCube (p, v) state)
            |> Map.ofSeq
            
        // Merge the new cubes in the original state
        state |> Map.union modifiedCubes
        
    let solution() =
        [1..6]
        |> Seq.fold (fun state _ -> applyRulesToState state) initialState
        |> Seq.map (fun kv -> kv.Value)
        |> Seq.filter id
        |> Seq.length
        |> Dump
 
Puzzle2.solution()