open System.Collections.Generic


#time
#load "../../Tools.fsx"

open System
open System.IO
open System.Text.RegularExpressions
open Checked
open Tools

type Jet = | Left | Right

// type Shape = | Minus | Plus | L | Bar | Square
// let shapeSequence = [| Minus; Plus; L; Bar; Square |]

let getInput p = 
    File.ReadAllText(getInputPath2022 p)
    |> Seq.map (fun c -> match c with | '<' -> Jet.Left | '>' -> Jet.Right | _ -> failwithf "Invalid input %c" c)
    |> Seq.toArray
    |> Dump

type Shape = {
    Name : string
    Blocks : (int64*int) list
    Width : int
    Height : int64
}

let minusShape = { Name = "Minus"; Blocks = [(0,0);(0,1);(0,2);(0,3)]; Width = 4; Height = 1 }
let plusShape = { Name = "Plus"; Blocks = [(0,1);(1,0);(1,1);(1,2);(2,1)]; Width = 3; Height = 3 }
let lShape = { Name = "L"; Blocks = [(0,0);(0,1);(0,2);(1,2);(2,2)]; Width = 3; Height = 3 }
let barShape = { Name = "Bar"; Blocks = [(0,0);(1,0);(2,0);(3,0)]; Width = 1; Height = 4 }
let squareShape = { Name = "Square"; Blocks = [(0,0);(0,1);(1,0);(1,1)]; Width = 2; Height = 2 }
let shapeSequence = [| minusShape; plusShape; lShape; barShape; squareShape |]

type Tunnel = {
    grid : HashSet<int64*int>
    highest : int64
}

let initTunnel() = 
    {
        grid = new HashSet<int64*int>()
        highest = 0L 
    }

let display (t:Tunnel) = 
    for r in t.highest .. -1L .. 0L  do
        printf "|"
        for c = 0 to 6 do
            printf "%c" (if t.grid.Contains(r,c) then '#' else '.')
        printfn "|"
    printfn "+-------+"

let doesShapeHitSomething shape (r,c) tunnel = 
    shape.Blocks |> Seq.exists(fun (br, bc) -> 
        tunnel.grid.Contains(r + br, c + bc)
    )
        
let moveShapeLeft shape (r,c) tunnel =
    if c = 0 then (r,c) 
    else if doesShapeHitSomething shape (r,c-1) tunnel then (r,c)
    else (r, c-1)

let moveShapeRight shape (r,c) tunnel =
    if c > (6 - shape.Width) then (r,c) 
    else if doesShapeHitSomething shape (r,c+1) tunnel then (r,c)
    else (r, c+1)
       
let canMoveShapeDown shape (r,c) tunnel =
    if r = 0L then false
    else 
        not (doesShapeHitSomething shape (r-1L,c) tunnel)
       
let freezeShape shape (r,c) tunnel =
    shape.Blocks |> Seq.iter (fun (br,bc) ->
        tunnel.grid.Add((r + br), (c + bc)) |> ignore
    )
    { tunnel with highest = max tunnel.highest (r + shape.Height - 1L )}

let rec loop maxRocks rockNb shapeIndex (r,c) tunnel (jets:Jet[]) index =
    if rockNb % 1_000_000L = 0L then
        printfn "Rock #%i out of %i falling" rockNb maxRocks
    if rockNb = maxRocks then
        tunnel
    else
        let jet = jets.[index % jets.Length]
        let shape = shapeSequence.[shapeIndex % shapeSequence.Length]
        let (rn,cn) = 
            match jet with
            | Jet.Left -> 
                moveShapeLeft shape (r,c) tunnel
            | Jet.Right -> 
                moveShapeRight shape (r,c) tunnel
        
        //printfn "Moved %s %A to %i,%i from %i,%i" shape.Name jet rn cn  r c
        
        // printfn "next post : %i,%i" rn cn

        if canMoveShapeDown shape (rn, cn) tunnel then
            //printfn "Moving %s down %i,%i" shape.Name (rn-1) cn
            loop maxRocks rockNb shapeIndex (rn-1L, cn) tunnel jets (index + 1)
        else
            //printfn "Freezing %s at %i,%i" shape.Name rn cn
            let nextTunnel = freezeShape shape (rn, cn) tunnel
            //printfn "Highest is now %i" nextTunnel.highest
            let pos = nextTunnel.highest + 3L + 1L, 2
            loop maxRocks (rockNb + 1L) (shapeIndex + 1) pos nextTunnel jets (index + 1)

let solve1 input =
    let instructions = getInput input
    let result = (loop 2023L 1L 0 (3, 2) (initTunnel()) instructions 0)
    result.highest + 1L

//solve1 "day17.txt"

//let solve2 input =
//    let instructions = getInput input
//    let result = (loop 1000_000_000_001L 1L 0 (3, 2) (initTunnel()) instructions 0)
//    result.highest + 1L
//solve2 "day17_sample1.txt"

// Not going to bruteforce it
// TODO Record state (jet index, shape index, height map for each column (adjusted to 0)
// TODO Find a cycle, compute the cycle state closest to the end
// TODO then let the rest play normally

(File.ReadAllText(getInputPath2022 "Day17.txt")).Length