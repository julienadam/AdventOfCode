<Query Kind="FSharpProgram" />

let toString : char seq -> string = Seq.map string >> String.concat ""
let getInputPath file = Path.Combine(Environment.GetFolderPath(Environment.SpecialFolder.UserProfile), @"CloudStation\Data\AdventOfCode\2016", file)
let path = getInputPath "day1.txt"
let input = File.ReadAllText(path) 

type Instruction =
| Left of int
| Right of int

let getInstructions input = 
    Regex.Matches(input, "(R\d+|L\d+)") 
    |> Seq.map (fun m -> 
        match m.Groups.[1].Value |> Seq.toList with
        | 'R' :: tl -> Right (tl |> toString |> int)
        | 'L' :: tl -> Left (tl |> toString |> int))

let move x y o d =
    match o with
    | 0 -> (x, y + d)
    | 180 -> (x, y - d)
    | 90 -> (x + d, y)
    | 270 -> (x - d, y)
    | a -> failwithf "Impossible angle %i" a
    
let inline taxiDistance (x,y) = abs x + abs y
    
let getNewOrientation orientation instruction =
    match instruction with
    | Right d -> (orientation + 90) % 360, d
    | Left d -> (orientation + 360 - 90) % 360, d
    
module Puzzle1 =
    let processInstruction (x,y,orientation) instruction =
        let o, d = getNewOrientation orientation instruction
        let (nx, ny) = move x y o d
        (nx, ny, o)
    
    let processInput input =
        let (x, y, _) = 
            getInstructions input 
            |> Seq.fold processInstruction (0,0,0)
        x, y
         
    let solution () = 
        //processInput "R2, L3" |> taxiDistance |> Dump
        //processInput "R2, R2, R2" |> taxiDistance |> Dump
        //processInput "R5, L5, R5, R3" |> taxiDistance |> Dump
        processInput input |> taxiDistance |> Dump
    
Puzzle1.solution()
    
module Puzzle2 =
    
    let moveAndTrace x y o d =
        match o with
        | 0 -> [1..d] |> List.map (fun i -> (x, y + i))
        | 180 -> [1..d] |> List.map (fun i -> (x, y - i))
        | 90 -> [1..d] |> List.map (fun i -> (x + i, y))
        | 270 -> [1..d] |> List.map (fun i -> (x - i, y))
        | a -> failwithf "Impossible angle %i" a
        
    let processInstruction (x,y,orientation,visited) instruction =
        let no, d = getNewOrientation orientation instruction
        let blocksWalked = moveAndTrace x y no d
        blocksWalked
        |> List.iter (fun (xv, yv) ->
            if visited |> Set.contains (xv, yv) then
                failwithf "Easter Bunny HQ is located at %i,%i, at a distance of %i" xv yv (taxiDistance (xv, yv)))
        
        let newVisited = Set.union (blocksWalked |> Set.ofSeq) visited
        let (lastX, lastY) = blocksWalked |> List.last
        (lastX, lastY, no, newVisited)
   
    let solution input = 
        getInstructions input 
        |> Seq.fold processInstruction (0,0,0, Set.empty |> Set.add (0,0))
    
Puzzle2.solution input