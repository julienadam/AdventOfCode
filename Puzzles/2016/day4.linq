<Query Kind="FSharpProgram" />

let toString : char seq -> string = Seq.map string >> String.concat ""
let getInputPath file = Path.Combine(Environment.GetFolderPath(Environment.SpecialFolder.UserProfile), @"CloudStation\Data\AdventOfCode\2016", file)
let path = getInputPath "day4.txt"

// let x = "aaaaa-bbb-z-y-x-123[abxyz]"
let parseRoom line =
    let m = Regex.Match(line, "(?<room>.*)\-(?<sector>\d+)\[(?<checksum>\w+)\]")
    m.Groups.["room"].Value.Split("-"), m.Groups.["sector"].Value |> int, m.Groups.["checksum"].Value
    
let validRooms = 
    File.ReadAllLines(path) 
    |> Seq.map parseRoom 
    |> Seq.map (fun (roomParts, sector, cs) ->
        let fiveMostCommonLettersOrdered = 
            roomParts 
            |> Seq.collect id 
            |> Seq.groupBy id
            |> Seq.sortByDescending (fun (g, letters) -> (letters |> Seq.length) * 1000 - (g |> int))
            |> Seq.map fst
            |> Seq.take 5
            |> toString
        roomParts, fiveMostCommonLettersOrdered, sector, cs)
    |> Seq.where (fun (_, ordered, _, cs) -> ordered = cs)

module Puzzle1 = 

    let solution() = 
        validRooms
        |> Seq.map (fun (_, _,x,_) -> x)
        |> Seq.sum
        |> Dump
        
printfn "Puzzle 1:"
Puzzle1.solution()

module Puzzle2 = 
    
    let aInt = ('a' |> int)
    
    let rotate (c:char) num =
        let dec = (c |> int) - aInt 
        (aInt + (dec + num) % 26) |> char
        
    let rotateString (s:seq<char>) num =
        s |> Seq.map (fun c -> rotate c num) |> Seq.toArray
        
        

    let solution() =
        validRooms
        |> Seq.map (fun (rooms, _, sectorId,_) -> 
            let rotated = 
                rooms |> Array.map (fun r -> 
                    rotateString r sectorId 
                    |> toString)
            String.Join(' ', rotated), sectorId
            )
        |> Seq.find (fun (s, _) -> s.Contains("north"))
        |> Dump

printfn "Puzzle 2:"
Puzzle2.solution()