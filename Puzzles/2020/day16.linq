<Query Kind="FSharpProgram" />

let inline tuple3 (array:'a[]) = array.[0], array.[1], array.[2]
let inline parseIntList (l:string) = l.Split(',') |> Array.map int
let inline groupVal (name:string) (m:Match) = m.Groups.[name].Value
let inline groupValInt name (m:Match) = m |> groupVal name |> int
let inline splitLines (s:string) = s.Split("\r\n")
let inline splitByEmptyLines (s:string) = s.Split("\r\n\r\n")
let inline matchAll predicate seq = (seq |> Seq.exists (fun s ->  (predicate s) |> not)) |> not

let getInputPath file = 
    Path.Combine(
        Environment.GetFolderPath(Environment.SpecialFolder.UserProfile), 
        @"CloudStation\Data\AdventOfCode\", 
        file)
        
let path = getInputPath "day16.txt"

type Interval = { Start: int; End: int }
let isInRangeInclusive range i = (i >= range.Start) && (i <= range.End)

[<NoComparison>]
type Field = { 
    Name: string
    Range1: Interval
    Range2: Interval 
}
with
    member this.IsInAnyRange i = i |> isInRangeInclusive this.Range1 || i |> isInRangeInclusive this.Range2

let parseField s =
    let m = Regex.Match(s, "^(?<name>[\w\s]+)\: (?<r1s>\d+)-(?<r1e>\d+) or (?<r2s>\d+)-(?<r2e>\d+)$")
    { 
        Name = m |> groupVal "name"
        Range1 = { Start = m |> groupValInt "r1s"; End = m |> groupValInt "r1e" }
        Range2 = { Start = m |> groupValInt "r2s"; End = m |> groupValInt "r2e" }
    }

let (fields, ticket, nearbyTickets) = 
    let f, t, n = 
        File.ReadAllText(path)
        |> splitByEmptyLines
        |> Array.map splitLines
        |> tuple3
    
    (f |> Array.map parseField, t |> Array.skip 1 |> Array.head |> parseIntList, n |> Array.skip 1 |> Array.map parseIntList)

// Puzzle 1

module Puzzle1 =

    let getInvalidValues (values: int seq) =
        values 
        |> Seq.filter (fun v -> 
            match fields |> Array.tryFind (fun f -> f.IsInAnyRange v) with
            | Some f -> 
                // printfn "%i is in range [%i..%i] or [%i..%i] of field %s" v f.Range1.Start f.Range1.End f.Range2.Start f.Range2.End f.Name
                false
            | None -> true
        )

    let solve () =
        nearbyTickets 
        |> Seq.collect getInvalidValues |> Dump
        |> Seq.sum
        |> printfn "Solution : %i"


module Puzzle2 =

    let allValuesAreValid (values: int seq) =
        values 
        |> Seq.exists (fun v -> 
            match fields |> Array.tryFind (fun f -> f.IsInAnyRange v) with
            | Some _ -> false
            | None -> true)
        |> not

    let rec assignFields (fieldCandidates: (Field * int list) list) (assigned: (Field * int) list) = 
        match fieldCandidates |> List.sortBy (fun (_, indices) -> indices.Length) with
        | [] -> assigned
        | hd::tl -> 
            match hd with
            | _, [] -> failwithf "Field with empty prospects"
            | f, i :: [] -> 
                // Le champ f a un seul indice possible, on l'assigne et on continue en retirant l'index des possibilités
                let newCandidates = tl |> List.map (fun (f, indices) -> (f, indices |> List.filter (fun index -> index <> i)))
                assignFields newCandidates ((f,i) :: assigned)
            | _, _ :: _ -> 
                failwithf "Not supported yet"
          
    let solve () =
        let validTickets = nearbyTickets |> Seq.filter allValuesAreValid
   
        let getIndicesThatAllMatchField (f:Field) =
            // liste des index pour lesquels la valeur de chaque ticket correspond au champ
            [0..ticket.Length - 1] 
            |> List.filter (fun i -> 
                validTickets 
                |> Seq.map (fun ticket -> ticket.[i])
                |> matchAll (fun v -> f.IsInAnyRange v))
        
        let fieldCandidates = 
            fields 
            |> Array.map (fun f -> f, getIndicesThatAllMatchField f)
            |> Array.toList
        
        let assignedFields = assignFields fieldCandidates []
        
        assignedFields 
        |> List.filter (fun (f, _) -> f.Name.StartsWith("departure")) 
        |> List.map (fun (_, i) -> ticket.[i] |> int64)
        |> List.fold (fun r v -> r * v) 1L
        |> Dump
        

// Puzzle1.solve()
Puzzle2.solve()

