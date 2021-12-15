<Query Kind="FSharpProgram" />

let inline splitLines (s:string) = s.Split("\r\n")
let inline splitByEmptyLines (s:string) = s.Split("\r\n\r\n")

let getInputPath file = Path.Combine(Environment.GetFolderPath(Environment.SpecialFolder.UserProfile), @"CloudStation\Data\AdventOfCode\", file)
let path = getInputPath "day20.txt"

let parseLine line =
    line 
    |> Seq.map (fun c -> 
        match c with 
        | '#' -> 1 
        | '.' -> 0 
        | x -> failwithf "Not a valid tile content char %c" x)

let parseTile tileText =
    let lines = tileText |> splitLines
    let lineNo = Regex.Match(lines.[0], "Tile (?<tile_no>\d+):").Groups.["tile_no"].Value |> int64
    let parsedLines = lines |> Array.skip 1 |> Array.map (parseLine >> Seq.toArray)
    let sideWidth = lines.[1].Length
    let array = Array2D.init sideWidth sideWidth (fun i j -> parsedLines.[i].[j])
    
    lineNo, array

module Puzzle1 =
    
    let boolsToInt bools = bools |> Array.mapi (fun i b -> b <<< i) |> Array.sum
    let boolsToIntRev bools = bools |> Array.rev |> Array.mapi (fun i b -> b <<< i) |> Array.sum
    
    let addTileSides (lineNo,(tile:int[,])) =
        let sideLength = (tile |> Array2D.length1) - 1
        let sides = [ 
            tile.[0, *]
            tile.[sideLength, *] |> Array.rev
            tile.[*, 0] |> Array.rev
            tile.[*, sideLength]
        ]
        //
        (lineNo, tile, sides |> List.map boolsToInt, sides |> List.map boolsToIntRev)
    
    
    let solution() = 
        let tiles = 
            File.ReadAllText path
            |> splitByEmptyLines
            |> Array.map parseTile
            |> Array.map addTileSides
            // |> Dump
        
        let corners =
            tiles 
            |> Array.map (fun (no, tile, sides, sidesRev) ->
                let otherTiles = tiles |> Seq.filter (fun (n, _, _, _) -> no <> n)
                let allOtherBordersRev = 
                    otherTiles 
                    |> Seq.collect (fun (_, _, sides, sidesRev) -> 
                        // Tiles can be flipped so we need the reversed and non reversed sides
                        sidesRev @ sides) 
                    |> Set.ofSeq
                let numUnmatchingBorders = 
                    sides 
                    |> Seq.filter (fun s -> allOtherBordersRev |> Seq.contains s |> not) 
                    |> Seq.length
                
                ((no, tile, sides, sidesRev), numUnmatchingBorders))
            |> Array.filter (fun (_, u) -> u = 2)
            |> Array.map fst
        
        corners |> Array.map (fun(n, _, _, _) -> n) |> Array.fold (*) 1L
    

// Puzzle1.solution() |> Dump

module Puzzle2 =

    let flip (ints: 'a[,]) =
        let side = (ints |> Array2D.length1) 
        Array2D.init side side (fun i j -> 
            Array2D.get ints (side - 1 - i) j)

    let rotate grid =
        let height, width = Array2D.length1 grid, Array2D.length2 grid
        Array2D.init width height (fun row column -> Array2D.get grid (height - column - 1) row)

    type Tile = {
        Id: int64
        TileContents: int[,]
    } with 
        member
            this.Flip() = { this with TileContents = flip this.TileContents }
        member
            this.Rotate() = { this with TileContents = rotate this.TileContents }

  
    let array2DEnum (array:'a[,]) = seq {
        for i=0 to (array |> Array2D.length1) - 1 do
            for j=0 to (array |> Array2D.length2) - 1 do
                yield (i,j), Array2D.get array i j
    }
        
    let findFreeSpace (grid:'a option[,]) =
        let ((x, y), _) = grid |> array2DEnum |> Seq.find (fun ((i,j), v) -> v.IsNone)
        (x,y)
        
    let getAllConfigurations (tile:Tile) =
        [
            tile; 
            tile.Rotate(); 
            tile.Rotate().Rotate(); 
            tile.Rotate().Rotate().Rotate();
            tile.Flip(); 
            tile.Flip().Rotate(); 
            tile.Flip().Rotate().Rotate(); 
            tile.Flip().Rotate().Rotate().Rotate();
        ]
        
    let compareSequences = Seq.compareWith Operators.compare
    
    let checkWithOption tile otherTile comparer =
        match tile, otherTile with
        | None, _ -> true
        | _, None -> true
        | Some t, Some o -> comparer t o
            
    let checkRight tile tileRight =
        checkWithOption tile tileRight (fun t r ->
            let side = t.TileContents |> Array2D.length1
            let m = t.TileContents.[*, side - 1]
            let o = r.TileContents.[*, 0]
            0 = compareSequences m o)
        
    let checkLeft tile tileLeft = checkRight tileLeft tile
        
    let checkUp tile tileUp = 
        checkWithOption tile tileUp (fun t r ->
            let side = t.TileContents |> Array2D.length1
            let m = t.TileContents.[0, *]
            let o = r.TileContents.[side - 1, *]
            0 = compareSequences m o)
        
    let checkDown tile tileDown = checkUp tileDown tile
    
    let getRightLeftDownUpChecks (grid:Tile option[,]) = seq {
        let side = grid |> Array2D.length1
        for i = 0 to side - 1 do
            for j = 0 to side - 1 do
                if i < side - 1 then
                    yield checkDown grid.[i,j] grid.[i+1,j]
                if i > 0 then
                    yield checkUp grid.[i,j] grid.[i-1,j]
                if j < side - 1 then
                    yield checkRight grid.[i,j] grid.[i,j+1]
                if j > 0 then
                    yield checkLeft grid.[i,j] grid.[i,j-1]
    }
                
    let isGridValid (grid:Tile option[,]) =
        getRightLeftDownUpChecks grid |> Seq.contains false |> not
                
    let rec placePiece (grid:Tile option[,]) (tiles:Tile list) =
        if not (isGridValid grid) then
            None
        else
            match tiles with
            | [] -> Some grid
            | t ->
                t |> Seq.tryPick (fun t ->
                    let tl = tiles |> List.except [t]
                    let (i,j) = findFreeSpace grid
                    let positions = getAllConfigurations t
                    positions |> Seq.tryPick (fun p -> 
                            let gridCopy = grid |> Array2D.copy
                            Array2D.set gridCopy i j (Some p)
                            placePiece gridCopy tl))

    let printMap map = 
        let mapSide = map |> Array2D.length1
        for i = 0 to mapSide - 1 do
            for j = 0 to mapSide - 1 do
                printf "%c" (match map.[i,j] with | 1 -> '#' | 0 -> '.' | _ -> failwith "foo")
            printfn ""
        
    let pattern =
        [
            (18,0);    
            (0,1);(5,1);(6,1);(11,1);(12,1);(17,1);(18,1);(19,1)
            (1,2);(4,2);(7,2);(10,2);(13,2);(16,2)
        ]
 
    let findPatternAt (map:int[,]) x y =
        pattern 
        |> Seq.exists (fun (i,j) -> 
            if map.[x + i, y + j] = 0 then
                // printfn "%i,%i was . instead of #" (x + i) (y + j)
                true
            else
                // printfn "%i,%i was #" (x + i) (y + j)
                false) 
        |> not
      
    let findPatternLocationsInMap map = seq {
        let side = map |> Array2D.length1
        for i = 0 to side - 1 - 20 do
            for j = 0 to side - 1 - 3 do
                if findPatternAt map i j then
                    yield (i,j)
    }
    
    let loadSamplePattern file =
        let parsed = 
            File.ReadAllLines(file)
            |> Array.map(fun l -> 
                l 
                |> Seq.map (fun c -> match c with | '#' -> 1 | '.' -> 0 | _ -> failwith "foo")
                |> Seq.toArray)
        let side = parsed.[0].Length
        Array2D.init side side (fun i j -> parsed.[j].[i])
            
    let solution () =
        let tiles = 
            File.ReadAllText path
            |> splitByEmptyLines
            |> Array.map parseTile
            |> Array.map (fun (n,t) -> { Id = n; TileContents = t })
            |> Array.toList
            
        let side = Math.Sqrt(tiles.Length |> float) |> int
        let p = Array2D.zeroCreate side side
        let solved = placePiece p tiles
        let pieceSide = tiles.[0].TileContents.GetLength 0
        
        let croppedTiles = solved.Value |> Array2D.map (fun tile ->
            { tile.Value with TileContents = tile.Value.TileContents.[1..pieceSide-2, 1..pieceSide-2]}
            )
        
        let finalCroppedMap =
            let croppedSide = pieceSide - 2
            let croppedMapSide = side * croppedSide
        
            Array2D.init croppedMapSide croppedMapSide (fun i j ->
                let subGrid = croppedTiles.[i / croppedSide, j / croppedSide]
                subGrid.TileContents.[i % croppedSide, j % croppedSide]
            )
        
        let totalPatternsFound = 
            [
                finalCroppedMap
                finalCroppedMap |> rotate
                finalCroppedMap |> rotate |> rotate
                finalCroppedMap |> rotate |> rotate |> rotate
                finalCroppedMap |> flip
                finalCroppedMap |> flip |> rotate
                finalCroppedMap |> flip |> rotate |> rotate
                finalCroppedMap |> flip |> rotate |> rotate |> rotate
            ] 
            |> Seq.collect findPatternLocationsInMap
            |> Seq.length
        
        let mutable totalHashes = 0;
        finalCroppedMap |> Array2D.iter (fun i -> totalHashes <- totalHashes + i)
        
        (totalHashes - (totalPatternsFound * pattern.Length)) |> Dump
                    
    // Translate pattern into coordinate diffs
    //                  # 
    //#    ##    ##    ###
    // #  #  #  #  #  #"   
        
    
    
    
Puzzle2.solution ()