<Query Kind="FSharpProgram" />

let getInputPath file = Path.Combine(Environment.GetFolderPath(Environment.SpecialFolder.UserProfile), @"CloudStation\Data\AdventOfCode\", file)
let path = getInputPath "day24.txt"

// https://www.redblobgames.com/grids/hexagons/
// Pointy-topped hex grid
// directions : e, se, sw, w, nw, and ne

type Direction = 
| East
| NorthEast
| NorthWest
| West
| SouthWest
| SouthEast

let parseDirection =
    function
    | "e" -> East
    | "ne" -> NorthEast
    | "nw" -> NorthWest
    | "w" -> West
    | "sw" -> SouthWest
    | "se" -> SouthEast
    | x -> failwithf "Invalid direction %s" x

let parseLine line = 
    let m = Regex.Match(line, "(se|sw|nw|ne|e|w)+")
    m.Groups.[1].Captures |> Seq.map (fun c -> parseDirection c.Value)
    
let input = File.ReadAllLines(path) |> Seq.map parseLine

// Using cube coordinates with 
// e = x+1, y-1 
// ne = x+1, z-1 
// nw = y+1, z-1 
// w = x-1, y+1 
// sw = x-1, z+1 
// se = y-1, z+1 
type HexCoord = { x: int; y: int; z: int } 
with static member Origin = { x=0; y=0; z=0 }

let move coord =
    function
    | East -> { coord with x=coord.x + 1; y=coord.y - 1 }
    | NorthEast -> { coord with x=coord.x + 1; z=coord.z - 1 }
    | NorthWest -> { coord with y=coord.y + 1; z=coord.z - 1 }
    | West -> { coord with x=coord.x - 1; y=coord.y + 1 }
    | SouthWest -> { coord with x=coord.x - 1; z=coord.z + 1 }
    | SouthEast -> { coord with y=coord.y - 1; z=coord.z + 1 }

let findTileFromline line = line |> Seq.fold move HexCoord.Origin

//"nwwswee" |> parseLine |> findTileFromline |> Dump
//"esew" |> parseLine |> findTileFromline |> Dump

let flipTiles input =
    let tiles = new Dictionary<HexCoord, bool>()
    input 
    |> Seq.map findTileFromline
    |> Seq.iter (fun coords ->
        match tiles.TryGetValue(coords) with
        | true, b -> tiles.[coords] <- not b
        | false, _ -> tiles.Add(coords, true)
    )
    tiles

module Puzzle1 =
    let solution() =
        flipTiles input |> Seq.filter(fun kvp -> kvp.Value) |> Seq.length |> Dump
        
Puzzle1.solution()
        
module Puzzle2 = 
    
    let getNeighbors coord =
        [
            move coord East
            move coord NorthEast
            move coord NorthWest
            move coord West
            move coord SouthWest
            move coord SouthEast
        ]

    let getTileColor coord (tiles: Dictionary<HexCoord, bool>) =
        match tiles.TryGetValue(coord) with
        | true, b -> b
        | false, _ -> false

    let getChangesAfterRules (tiles: Dictionary<HexCoord, bool>) =
        // Get all current known tiles, and their neighbors
        // So we can apply the rules on all potentially affected tiles
        let allCoordsToCheck = 
            tiles
            |> Seq.collect (fun kvp -> kvp.Key :: (getNeighbors kvp.Key))
            |> Seq.distinct
            |> Seq.toList
        
        allCoordsToCheck |> List.choose (fun c ->
            let numBlackNeighbors = 
                getNeighbors c
                |> List.map (fun c -> getTileColor c tiles)
                |> List.where id 
                |> List.length
                
            // Any black tile with zero or more than 2 black tiles immediately adjacent to it is flipped to white.
            // Any white tile with exactly 2 black tiles immediately adjacent to it is flipped to black.
            match tiles |> getTileColor c, numBlackNeighbors with
            | true, 0 -> Some (c, false)
            | true, x when x > 2 -> Some (c, false)
            | false, 2 -> Some (c, true)
            | _ -> None
        )
    
    let solution() =
        let tiles = flipTiles input
        
        let days = 100
        for i = 1 to days do
            let changes = getChangesAfterRules tiles
            changes |> Seq.iter (fun (coords, v) -> tiles.[coords] <- v)
            let blackTiles = tiles |> Seq.filter(fun kvp -> kvp.Value) |> Seq.length
            printfn "Black tiles after %i days : %i" i blackTiles
        
        
Puzzle2.solution()