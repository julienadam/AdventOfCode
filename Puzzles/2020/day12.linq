<Query Kind="FSharpProgram" />

let path = 
    Path.Combine(
        Environment.GetFolderPath(Environment.SpecialFolder.UserProfile), 
        "CloudStation\Data\AdventOfCode\day12.txt")
let input = File.ReadAllLines(path)

type Degrees = int

type Direction =
    | North
    | South
    | East
    | West

type Instruction =
    | Move of Direction * int
    | Forward of int
    | Rotate of Degrees

let parseLine line =
    let m = Regex.Match (line, "(?<ins>N|S|E|W|F|L|R)(?<units>\d+)")
    if not m.Success then failwithf "could not parse %s" line
    
    let units = m.Groups.["units"].Value |> int
    match m.Groups.["ins"].Value with
    | "N" -> Move (North, units)
    | "S" -> Move (South, units)
    | "E" -> Move (East, units)
    | "W" -> Move (West, units)
    | "F" -> Forward units
    | "L" -> Rotate (-units)
    | "R" -> Rotate units
    | c -> failwithf "Unknown instruction code %s" c
    
let instructions = input |> Seq.map parseLine
    
module Puzzle1 =
    type State = {
        NSPos: int
        EWPos: int
        Heading: Degrees
    } with 
        member 
            this.Forward units =
                match this.Heading with 
                | 0 -> { this with NSPos = this.NSPos + units }
                | 180 -> { this with NSPos = this.NSPos - units }
                | 90 -> { this with EWPos = this.EWPos + units }
                | 270 -> { this with EWPos = this.EWPos - units  }
                | d -> failwithf "Invalid angle %i" d
        static member Initial = { NSPos = 0; EWPos = 0; Heading = 90 }
    
    let processInstruction state instruction =
        let result = 
            match instruction with
            | Rotate x -> { state with Heading = (state.Heading + x + 360) % 360 }
            | Move (North, units) -> { state with NSPos = state.NSPos + units }
            | Move (South, units) -> { state with NSPos = state.NSPos - units }
            | Move (East, units) -> { state with EWPos = state.EWPos + units }
            | Move (West, units) -> { state with EWPos = state.EWPos - units }
            | Forward units -> state.Forward units
        printfn "%A on (%i, %i) heading %i -> (%i, %i) heading %i" instruction state.NSPos state.EWPos state.Heading result.NSPos result.EWPos result.Heading
        result
        
    let solution () =
        let finalState = instructions |> Seq.fold processInstruction State.Initial
        ((abs finalState.EWPos) + (abs finalState.NSPos))

module Puzzle2 =
    type State = {
        NSPos: int
        EWPos: int
        WaypointNSPos: int
        WaypointEWPos: int
    } with 
        member 
            this.Forward units = { this with NSPos = this.NSPos + (this.WaypointNSPos * units); EWPos = this.EWPos + (this.WaypointEWPos * units) }
        member
            this.Rotate degrees = 
                match degrees with
                | 90 -> { this with WaypointEWPos = this.WaypointNSPos; WaypointNSPos = -this.WaypointEWPos}
                | 180 -> { this with WaypointEWPos = -this.WaypointEWPos; WaypointNSPos = -this.WaypointNSPos}
                | 270 -> { this with WaypointEWPos = -this.WaypointNSPos; WaypointNSPos = this.WaypointEWPos}
                | d -> failwithf "Unsupported rotation %d" d
                    
        static member Initial = { NSPos = 0; EWPos = 0; WaypointNSPos = 1; WaypointEWPos = 10 }
    
    let processInstruction (state:State) instruction =
        let result = 
            match instruction with
            | Rotate degrees -> state.Rotate ((degrees + 360) % 360)
            | Move (North, units) -> { state with WaypointNSPos = state.WaypointNSPos + units }
            | Move (South, units) -> { state with WaypointNSPos = state.WaypointNSPos - units }
            | Move (East, units) -> { state with WaypointEWPos = state.WaypointEWPos + units }
            | Move (West, units) -> { state with WaypointEWPos = state.WaypointEWPos - units }
            | Forward units -> state.Forward units
        printfn "%A on (%i, %i) (%i, %i) -> (%i, %i) (%i, %i)" instruction state.NSPos state.EWPos state.WaypointNSPos state.WaypointEWPos result.NSPos result.EWPos result.WaypointNSPos result.WaypointEWPos
        result
        
    let solution () = 
        let finalState = instructions |> Seq.fold processInstruction State.Initial
        ((abs finalState.EWPos) + (abs finalState.NSPos))

Puzzle2.solution() |> Dump