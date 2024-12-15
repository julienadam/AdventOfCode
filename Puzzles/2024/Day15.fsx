#time "on"
#load "../../Tools.fs"
#load "../../Tools/Array2DTools.fs"
#r "nuget: NFluent"

open System
open System.IO
open AdventOfCode
open Array2DTools
open NFluent

let getInput name = 
    let gridRaw,movesRaw = File.ReadAllText(getInputPath2024 name).Replace("\r", "") |> ssplit "\n\n" |> tupleize2
    gridRaw |> splitLines, movesRaw.Replace("\r", "").Replace("\n", "")

let mapMoves = 
    function
    | '^' -> Array2DTools.tryGetUp
    | 'v' -> Array2DTools.tryGetDown
    | '>' -> Array2DTools.tryGetRight
    | '<' -> Array2DTools.tryGetLeft
    | x -> failwithf "Not a valid move '%c'" x

/// try to move in the direction provided
/// returns the move if it's possible, or an empty list
/// it's recusive so if it finds a box in its way it will try to move it
/// and this box will also try to move etc.
// the final result is a list of moves triggered by the initial move
let rec tryMove (r,c) (grid:char[,]) move =
    match grid |> move r c with
    | Some (ar, ac, av) when av = '.' -> [(r,c),(ar,ac)]
    | Some (ar, ac, av) when av = 'O' -> 
        match tryMove (ar,ac) grid move with
        | [] -> []
        | x -> ((r,c),(ar,ac))::x
    | Some (_, _, av) when av = '#' -> []
    | None -> []
    | x -> failwithf "Invalid cell contents %A" x

/// applies all the moves in reverse order and returns the
/// final position of the robot
let applyMoves (moves:((int*int)*(int*int))seq) (grid:char[,])=
    moves 
    |> Seq.rev
    |> Seq.iter (fun ((sr,sc),(er,ec)) ->
        grid[er,ec] <- grid[sr,sc]
        grid[sr,sc] <- '.')

    moves |> Seq.tryHead |> Option.map snd

let gpsCoords r c = r*100+c

let solve1 input =
    let grid, moves = getInput input
    let grid = grid |> array2D
    let (rr,rc,_) =  grid |> Array2DTools.filteri (fun _ _ v -> v = '@') |> Seq.head

    let mutable r = rr
    let mutable c = rc

    for m in (moves |> Seq.map mapMoves) do
        match applyMoves (tryMove (r,c) grid m) grid with
        | Some (nr,nc) -> r <- nr; c <- nc
        | None -> ()
    // printGrid grid
    
    // Calculate gps coords for all boxes
    grid 
    |> filteri (fun _ _ v -> v = 'O')
    |> Seq.map (fun (r,c,_) -> gpsCoords r c)
    |> Seq.sum

Check.That(solve1 "Day15_sample1.txt").Equals(10092)
Check.That(solve1 "Day15_sample2.txt").Equals(2028)

solve1 "Day15.txt"

let expandWarehouse (input:string) = input.Replace("#", "##").Replace(".", "..").Replace("O", "[]").Replace("@", "@.")

/// try to move in the direction provided
/// returns the move if it's possible, or an empty list
/// it's recusive so if it finds a box in its way it will try to move it
/// and this box will also try to move etc.
// the final result is a list of moves triggered by the initial move
// handles the larger boxes
let rec tryMove2 (r,c) (grid:char[,]) moveFunc move =
    //match m with
    //| '<' -> tryMove (r,c) grid Array2DTools.tryGetLeft
    //| '>' -> tryMove (r,c) grid Array2DTools.tryGetRight
    //| '^' -> 
    //| 'v' -> 
    
    match grid |> moveFunc r c with
    | Some (ar, ac, av) when av = '.' -> [(r,c),(ar,ac)]
    | Some (ar, ac, av) when av = ']' -> 
        match move with 
        | '^'
        | 'v' ->
            match (tryMove2 (ar,ac) grid moveFunc move), (tryMove2 (ar,ac-1) grid moveFunc move) with
            | [],_
            | _,[] -> []
            | (half1,half2) -> ((r,c),(ar,ac))::(List.concat [half1; half2])
        | _ ->
            match tryMove2 (ar,ac) grid moveFunc move with
            | [] -> []
            | x -> ((r,c),(ar,ac))::x

    | Some (ar, ac, av) when av = '[' -> 
        match move with 
        | '^'
        | 'v' ->
            match (tryMove2 (ar,ac) grid moveFunc move), (tryMove2 (ar,ac+1) grid moveFunc move) with
            | [],_
            | _,[] -> []
            | (half1,half2) -> ((r,c),(ar,ac))::(List.concat [half1; half2])
        | _ ->
            match tryMove2 (ar,ac) grid moveFunc move with
            | [] -> []
            | x -> ((r,c),(ar,ac))::x
    | Some (_, _, av) when av = '#' -> []
    | None -> []
    | x -> failwithf "Invalid cell contents %A" x

/// applies all the moves in reverse order and returns the
/// final position of the robot, removes duplicate moves
/// caused by the larger boxes
let applyMoves2 (moves:((int*int)*(int*int))seq) (grid:char[,])=
    moves 
    |> Seq.rev
    |> Seq.distinct
    |> Seq.iter (fun ((sr,sc),(er,ec)) ->
        grid[er,ec] <- grid[sr,sc]
        grid[sr,sc] <- '.')

    moves |> Seq.tryHead |> Option.map snd


let solve2 input =
    let grid, moves = getInput input
    let grid = grid |> Seq.map expandWarehouse |> array2D
    let (rr,rc,_) =  grid |> Array2DTools.filteri (fun _ _ v -> v = '@') |> Seq.head

    let mutable r = rr
    let mutable c = rc

    for m in moves do
        match applyMoves2 (tryMove2 (r,c) grid (m |> mapMoves) m) grid with
        | Some (nr,nc) -> r <- nr; c <- nc
        | None -> ()

    // Calculate gps coords for all boxes
    grid 
    |> filteri (fun _ _ v -> v = '[')
    |> Seq.map (fun (r,c,_) -> gpsCoords r c)
    |> Seq.sum

Check.That(solve2 "Day15_sample1.txt").Equals(9021)

solve2 "Day15_sample1.txt"
