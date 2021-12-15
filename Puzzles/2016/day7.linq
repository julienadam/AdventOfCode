<Query Kind="FSharpProgram" />

let toString : char seq -> string = Seq.map string >> String.concat ""
let getInputPath file = Path.Combine(Environment.GetFolderPath(Environment.SpecialFolder.UserProfile), @"CloudStation\Data\AdventOfCode\2016", file)
let path = getInputPath "day7.txt"

type Oddity = |Even |Odd
    with static member FromNumber i = if i % 2 = 0 then Odd else Even

module Puzzle1 =
    
    let hasValidAbba (input:string) =
        input 
        |> Seq.windowed 4
        |> Seq.exists(fun w -> (w.[0] = w.[3]) && (w.[1] = w.[2]) && (w.[0] <> w.[1]))

    let tryValidateIPV7 (address:string) = 
        let mapped = 
            address.Split('[', ']') 
            |> Seq.mapi (fun i part -> Oddity.FromNumber i, hasValidAbba part)
        let hasAbbaInsideBrackets = 
            mapped 
            |> Seq.where (fun (oddity, _) -> oddity = Even)
            |> Seq.exists snd
            
        if hasAbbaInsideBrackets then
            None
        else
            let hasAbbaOutsideBrackets =
                mapped 
                |> Seq.where (fun (oddity, _) -> oddity = Odd)
                |> Seq.exists snd
            if hasAbbaOutsideBrackets then
                Some address
            else 
                None
         
    let solution() =
        File.ReadAllLines(path)
        |> Seq.choose tryValidateIPV7
        |> Seq.length
        |> Dump
        
Puzzle1.solution()


module Puzzle2 =
    
    let getValidAbas (input:string) =
        input 
        |> Seq.windowed 3
        |> Seq.filter (fun window -> (window.[0] = window.[2]) && (window.[0] <> window.[1]))
        |> Seq.map toString
        
    let tryValidateSslEnabledIPV7 (address:string) = 
        let mapped = 
            address.Split('[', ']') 
            |> Seq.mapi (fun i part -> Oddity.FromNumber i, getValidAbas part)
        
        let supernetAbas = 
            mapped 
            |> Seq.where (fun (oddity, _) -> oddity = Odd)
            |> Seq.collect snd
            |> Set.ofSeq
        
        let hypernetAbas =
            mapped 
            |> Seq.where (fun (oddity, _) -> oddity = Even)
            |> Seq.collect snd
            |> Set.ofSeq
        
        let invertedHypernetAbas = 
            hypernetAbas 
            |> Set.map (fun s -> [ s.[1]; s.[0]; s.[1] ] |> toString)
            
        let intersection = Set.intersect supernetAbas invertedHypernetAbas
        if intersection |> Set.isEmpty then
            None
        else
            Some address
    
    let solution() =
        File.ReadAllLines(path)
        |> Seq.choose tryValidateSslEnabledIPV7
        |> Seq.length
        |> Dump
        
Puzzle2.solution()