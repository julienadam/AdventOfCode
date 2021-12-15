<Query Kind="FSharpProgram" />

let inputPath = @"C:\Users\Arcodia\CloudStation\Data\AdventOfCode\day2.txt"
let r = new Regex("^(\d+)-(\d+) (\w): (\w+)$")

type PasswordEntry =
    {
        MinChars: int
        MaxChars: int
        Char: char
        Password: string
    }

let parseLine l = 
    let m = r.Match(l)
    {
        MinChars = m.Groups.[1].Value |> int
        MaxChars = m.Groups.[2].Value |> int
        Char = m.Groups.[3].Value.[0]
        Password = m.Groups.[4].Value
    }

let s1 () =
    
    let validate entry =
        match entry.Password |> Seq.where (fun c -> c = entry.Char) |> Seq.length with
        | chars when chars < entry.MinChars -> 
            printfn "%s has too few %c's, found %i expected %i" entry.Password entry.Char chars entry.MinChars
            None
        | chars when chars > entry.MaxChars -> 
            printfn "%s has too many %c's, found %i expected %i" entry.Password entry.Char chars entry.MaxChars
            None
        | _ -> Some entry
            
    File.ReadAllLines(inputPath) 
    |> Seq.map parseLine 
    |> Seq.choose validate
    |> Seq.length
    |> Dump
    

let s2 () =
    
    let validate entry =
        match (entry.Password.[entry.MinChars - 1], entry.Password.[entry.MaxChars - 1]) with
        | c1, c2 when c1 = entry.Char && c2 = entry.Char -> None
        | c, _ when c = entry.Char -> Some entry
        | _, c when c = entry.Char -> Some entry
        | _, _ -> None
            
    File.ReadAllLines(inputPath) 
    |> Seq.map parseLine 
    |> Seq.choose validate
    |> Seq.length
    |> Dump

// s1()

s2()