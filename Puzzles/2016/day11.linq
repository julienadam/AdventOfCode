<Query Kind="FSharpProgram" />

let toString : char seq -> string = Seq.map string >> String.concat ""
let getInputPath file = Path.Combine(Environment.GetFolderPath(Environment.SpecialFolder.UserProfile), @"CloudStation\Data\AdventOfCode\2016", file)
let path = getInputPath "day11_sample.txt"

type Item =
| Generator of string
| Microchip of string

let parseRank input =
    match input with
    | "first" -> 1
    | "second" -> 2
    | "third" -> 3
    | "fourth" -> 4
    | s -> failwithf "Invalid rank %s" s

let cleanContent (input:string) =
    let r = input.Trim([|' '; '.'|])
    if r.StartsWith("and ") then
        r.Substring(4)
    else
        r

let parseItem (input:string) =
    let m = Regex.Match(input, "a (?<gen>\w+) generator|a (?<chip>\w+)-compatible microchip")
    if m.Success then
        match m.Groups.["gen"].Value, m.Groups.["chip"].Value with
        | "", x -> Microchip x
        | x, "" -> Generator x
        | _ -> failwithf "Could not recognize item %s as a chip or a generator" input
    else
        failwithf "Not a valid item %s" input
    
let parseContent (input:string) =
    if input = " nothing relevant" then
        [||]
    else
        input.Split(',') 
        |> Array.map (cleanContent >> parseItem)

let parseLine line = 
    let m = Regex.Match(line, "The (\w+) floor contains( nothing relevant| [^\.]+)")
    if m.Success then
        m.Groups.["1"].Value |> parseRank, m.Groups.["2"].Value |> parseContent
    else
        failwithf "Not a valid line %s" line

//File.ReadAllLines(path)
//|> Array.map parseLine
// |> Dump
//
//type IsotopeEquipement =
//| Generator of string
//| Microchip of string
//| PoweredChip of string

let hasLoneChip items = 
        items
        |> List.exists(fun e -> 
            match e with
            | Microchip a -> 
                items 
                |> List.exists(fun x -> match x with | Generator x when x = a -> true | _ -> false)
                |> not
            | _ -> false)

let hasHazardousMaterial items = 
    items 
    |> List.exists(fun e -> 
        match e with 
        | Generator _ -> true
        | _ -> false)

let isFloorIrradiated (items: Item list) = 
    (items |> hasLoneChip) && (items |> hasHazardousMaterial)

type Elevator = {
    Floor: int
    Item1 : Item option
    Item2 : Item option
} with 
    member this.CanMove() =
        match this.Item1, this.Item2 with
        | Some _, _ -> true
        | _, Some _ -> true
        | _ -> false
    member this.IsIrradiated() =
        match this.Item1, this.Item2 with
        | Some _, None -> false
        | None, Some _ -> false
        | Some(Generator x), Some(Microchip y) when x = y -> false
        | Some(Microchip x), Some(Generator y) when x = y -> false
        | Some(Microchip _), Some(Microchip _) -> false
        | _ -> true
    member this.IsEmpty() = this.Item1.IsNone && this.Item2.IsNone
//
//{ Floor = 0; Item1 = Some(Microchip "a"); Item2 = Some(Generator "b") }.IsIrradiated().Dump()
//{ Floor = 0; Item1 = Some(Microchip "a"); Item2 = Some(Microchip "b") }.IsIrradiated().Dump()
//{ Floor = 0; Item1 = Some(Microchip "a"); Item2 = Some(Generator "a") }.IsIrradiated().Dump()
//{ Floor = 0; Item1 = Some(Microchip "a"); Item2 = None }.IsIrradiated().Dump()
//{ Floor = 0; Item1 = None; Item2 = Some(Generator "a") }.IsIrradiated().Dump()

    

type State = {
    Elevator : Elevator
    Floors : List<int * Item list>
}

let is

let allFloorsEmptyExcept4th (floors: List<int * Item list>) =
    let filtered = 
        floors
        |> List.map (fun (k,v) ->
            match (k, v) with
            | 4, _ -> true
            | x, [] -> true
            | x, _ -> false)
    filtered |> List.fold (&&) true

let isEndState state =
    state.Elevator.IsEmpty() && (state.Floors |> allFloorsEmptyExcept4th)
          

isEndState

//let checkState (state:State) =
//    // Check all floors to see if they are irradiated
//    if state.Floors |> List.find (fun (_,v) -> v |> isFloorIrradiated) then
//        FailedDueToIrradiation
//    else if isEndState state then
//        Final
//    else 
//        Intermediate
//    
//// Store a dictionary of known states to avoid loops
//// Recursively move stuff and see if it fails, starting by moving stuff up
////
////
////let getNextMoves state = seq {
////    
////    yield { state with E
////}
////
////let rec Turn (state:State) turns =
////    match checkState state with
////    | Final -> Some turns
////    | FailedDueToIrradiation -> None
////    | Intermediate ->
////        // Calculate next possible moves and recurse
////        // Turn
//    
//        
//        
//
//// Rules
//// 1. Elevator needs at least one G or MC
//// 2. Elevator can contain at most 2 G or MC
//// 3. GA + MCA = Powered A. GA still dangerous to MCB but not to Powered B
//// 4. 
//