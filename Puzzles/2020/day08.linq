<Query Kind="FSharpProgram" />

let path = 
    Path.Combine(
        Environment.GetFolderPath(Environment.SpecialFolder.UserProfile), 
        "CloudStation\Data\AdventOfCode\day8.txt")
let input = File.ReadAllLines(path)

type Instruction =
    | Nop of int
    | Acc of int
    | Jmp of int

let parseInstruction line = 
    match Regex.Match(line, "^(?<ins>nop|acc|jmp) (?<off>[+-]\d+)") with
    | m when m.Success -> 
        match m.Groups.["ins"].Value with
        | "nop" -> Instruction.Nop (m.Groups.["off"].Value |> int)
        | "acc" -> Instruction.Acc (m.Groups.["off"].Value |> int)
        | "jmp" -> Instruction.Jmp (m.Groups.["off"].Value |> int)
        | x -> failwithf "Unknown instruction %s" x
    | _ -> failwithf "Failed to parse line %s" line
    
type State = {
    Accumulator: int
    Visited: int Set
} with static member Default = { Accumulator = 0; Visited = Set.empty }
    
let s1 () =
    
    let rec processInstructions (state:State) (index:int) (instructions: Instruction[]) =
        match state.Visited.Contains(index) with
        | true -> state.Accumulator
        | false ->
            let instr = instructions.[index]
            let nextState = { state with Visited = state.Visited.Add(index) }
            match instr with
            | Nop _ -> 
                processInstructions nextState (index + 1) instructions
            | Acc i -> 
                processInstructions { nextState with Accumulator = nextState.Accumulator + i } (index + 1) instructions
            | Jmp i ->
                processInstructions nextState (index + i) instructions

    input 
    |> Array.map parseInstruction
    |> processInstructions State.Default 0
    
printfn "Puzzle 1"
s1 () |> Dump
    
let s2 () =
    let invert instruction =
        match instruction with
        | Jmp x -> Nop x
        | Nop x -> Jmp x
        | Acc _ -> failwithf "shouldn't have process a fix for an Acc"
        
    let rec processInstructionsAndUncorrupt (state:State) (index:int) (tryFixAt:int) (instructions: Instruction[]) =
        if index >= instructions.Length then
            Some state.Accumulator
        else
            match state.Visited.Contains(index) with
            | true -> None
            | false ->
                let instr = if index = tryFixAt then instructions.[index] |> invert else instructions.[index]
                let nextState = { state with Visited = state.Visited.Add(index) }
                match instr with
                | Nop _ -> 
                    processInstructionsAndUncorrupt nextState (index + 1) tryFixAt instructions
                | Acc i -> 
                    processInstructionsAndUncorrupt { nextState with Accumulator = nextState.Accumulator + i } (index + 1) tryFixAt instructions
                | Jmp i ->
                    processInstructionsAndUncorrupt nextState (index + i) tryFixAt instructions
    

    let instructions = input |> Array.map parseInstruction
    
    let modsIndices = 
        instructions
        |> Array.mapi (fun i instruction -> i, match instruction with | Nop _ -> true | Jmp _ -> true | _ -> false )
        |> Array.where snd
        |> Array.map fst
        
    modsIndices |> Array.pick (fun fixAt  -> processInstructionsAndUncorrupt State.Default 0 fixAt instructions)
    

printfn "Puzzle 2"
s2 () |> Dump