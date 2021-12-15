<Query Kind="FSharpProgram" />

let toString : char seq -> string = Seq.map string >> String.concat ""
let inline mInt (name:string) (m:Match) = m.Groups.[name].Value |> int
let getInputPath file = Path.Combine(Environment.GetFolderPath(Environment.SpecialFolder.UserProfile), @"CloudStation\Data\AdventOfCode\2016", file)
let path = getInputPath "day8.txt"

type Instructions =
| Rect of int * int
| RotateRow of int * int
| RotateColumn of int * int

let regexes = [
    "rect (\d+)x(\d+)", Rect
    "rotate row y=(\d+) by (\d+)", RotateRow
    "rotate column x=(\d+) by (\d+)", RotateColumn]

let parseLine line =
    regexes |> List.pick(fun (r, f) -> 
        let m = Regex.Match(line, r)
        if m.Success then 
            let prms = (m |> mInt "1"),(m |> mInt "2")
            Some (f prms)
        else None)
        
let instructions = 
    File.ReadAllLines(path)
    |> Array.map parseLine  

let initialScreen = Array2D.create 6 50 false

let processInstruction (screen:bool[,]) = function
    | Rect (a,b) -> 
        let toBlit = Array2D.create b a true
        Array2D.blit toBlit 0 0 screen 0 0 b a
        screen
    | RotateRow (row, nbPixels) ->
        let rowPixels = screen.[row, *]
        rowPixels 
        |> Array.permute (fun i -> (i + nbPixels) % rowPixels.Length)
        |> Array.iteri (fun i v -> screen.[row, i] <- v)
        screen
    | RotateColumn (column, nbPixels) ->
        let columnPixels = screen.[*, column]
        columnPixels
        |> Array.permute (fun i -> (i + nbPixels) % columnPixels.Length)
        |> Array.iteri (fun i v -> screen.[i, column] <- v)
        screen
  
let iterArray2D a = seq { 
    for x in [0..(Array2D.length1 a) - 1] do 
        for y in [0..(Array2D.length2 a) - 1] do 
            yield a.[x, y] }

let finalScreen = 
    instructions 
    |> Seq.fold processInstruction initialScreen
    
    
let puzzle1 =
    finalScreen
    |> iterArray2D
    |> Seq.where id
    |> Seq.length
    |> Dump

for i = 0 to (finalScreen |> Array2D.length1) - 1 do
    for j = 0 to (finalScreen |> Array2D.length2) - 1 do
        printf "%c" (if finalScreen.[i, j] then '█' else ' ')
    printfn ""
    
let puzzle2 = "ZJHRKCPLYJ"