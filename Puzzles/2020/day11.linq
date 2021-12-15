<Query Kind="FSharpProgram" />

let path = 
    Path.Combine(
        Environment.GetFolderPath(Environment.SpecialFolder.UserProfile), 
        "CloudStation\Data\AdventOfCode\day11.txt")
let input = File.ReadAllLines(path)

type Space =
    | Floor
    | Empty
    | Occupied

let inline parseSpace c =
    match c with
    | 'L' -> Space.Empty
    | '.' -> Space.Floor
    | '#' -> Space.Occupied
    | c -> failwithf "%c is not a valid seating symbol" c

let initialMap = 
    Array2D.init input.[0].Length input.Length (fun i j -> parseSpace input.[j].[i])

let printSeatMap (array) =
    
    let sb = new StringBuilder()
    for j = 0 to (array |> Array2D.length2) - 1 do
        for i = 0 to (array |> Array2D.length1) - 1 do
            sb.Append(match Array2D.get array i j with | Occupied -> '#' | Empty -> 'L' | Floor -> '.') |> ignore
        sb.AppendLine() |> ignore
    
    sb.DumpFixed() // Extension that outputs in fixed size in Linqpad

let getAdjacentSeats i j array =
    let minI = max (i - 1) 0
    let maxI = min (i + 1) ((array |> Array2D.length1) - 1)
    let minJ = max (j - 1) 0
    let maxJ = min (j + 1) ((array |> Array2D.length2) - 1)
    
    List.allPairs [minI .. maxI] [minJ .. maxJ]
    |> List.where (fun (x,y) -> x <> i || y <> j) // Skip self
    |> List.map (fun (x,y) -> Array2D.get array x y)
        
let getOccupiedAdjacentSeats i j array =
    getAdjacentSeats i j array 
    |> List.where (fun space -> match space with | Occupied -> true | _ -> false)
    |> List.length

let rec fill array =
    let mutable hasChanges = false
    let next = array |> Array2D.mapi(fun i j space -> 
        let occupied = array |> getOccupiedAdjacentSeats i j
        match occupied, space with
        | 0, Empty -> 
            hasChanges <- true
            Space.Occupied
        | i, Occupied when i >= 4 -> 
            hasChanges <- true
            Space.Empty
        | _, x -> x
        )
        
    // next |> printSeatMap |> ignore
        
    if hasChanges then
        fill next
    else
        next

let flat2Darray array2D = seq { 
    for x in [0..(Array2D.length1 array2D) - 1] do 
        for y in [0..(Array2D.length2 array2D) - 1] do 
            yield array2D.[x, y] }

let s1 () = fill initialMap |> flat2Darray |> Seq.where (fun c -> c = Space.Occupied) |> Seq.length

s1() |> Dump

let rec getFirstVisibleSeat i j offI offJ array =
    let nextI = i + offI
    let nextJ = j + offJ
    if nextI < 0 || nextI > (array |> Array2D.length1) - 1 || nextJ < 0 || nextJ > (array |> Array2D.length2) - 1 then 
        None
    else
        match array.[nextI, nextJ] with
        | Floor -> getFirstVisibleSeat nextI nextJ offI offJ array
        | x -> Some x
    
let getVisibleOccupiedSeats i j array =
    [(-1, -1); (-1, 0); (0, -1); (1, 1); (1, 0); (0, 1); (-1, 1); (1, -1)]
    |> Seq.sumBy(fun (offI, offJ) -> 
        let o = getFirstVisibleSeat i j offI offJ array
        match o with 
        | Some Occupied -> 1
        | _ -> 0)
    
let rec fill2 array =
    let mutable hasChanges = false
    let next = array |> Array2D.mapi(fun i j space -> 
        let occupied = array |> getVisibleOccupiedSeats i j
        match occupied, space with
        | 0, Empty -> 
            hasChanges <- true
            Space.Occupied
        | i, Occupied when i >= 5 -> 
            hasChanges <- true
            Space.Empty
        | _, x -> x
        )
        
    // next |> printSeatMap |> ignore
        
    if hasChanges then
        fill2 next
    else
        next
        

let s2 () = fill2 initialMap |> flat2Darray |> Seq.where (fun c -> c = Space.Occupied) |> Seq.length

s2() |> Dump