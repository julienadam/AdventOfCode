<Query Kind="FSharpProgram" />

let path = 
    Path.Combine(
        Environment.GetFolderPath(Environment.SpecialFolder.UserProfile), 
        "CloudStation\Data\AdventOfCode\day7.txt")
let input = File.ReadAllLines(path)

let r = new Regex("^(?<outer_color>[\w]+ [\w]+) bags contain (?<inner_bags>(((?<num>\d+) (?<inner_color>[\w]+ [\w]+)) bags?(, |\.))+|no other bags)")

let rules = 
    input 
    |> Seq.map (fun line -> r.Match(line)) 
    |> Seq.map (fun m -> 
        m.Groups.["outer_color"].Value, 
        match m.Groups.["inner_bags"].Value with 
        | "no other bags" -> [] 
        | _ -> 
            (m.Groups.["inner_color"].Captures |> Seq.map (fun c -> c.Value))
            |> Seq.zip (m.Groups.["num"].Captures |> Seq.map (fun c -> c.Value))
            |> Seq.toList
            )
    |> Map.ofSeq

let rec walk num (color:string) =
    let sub =
        rules.[color]
        |> List.collect (fun (n, color) -> walk ((int) n) color)
    (num, color) :: sub
    
let walks = rules |> Map.map (fun k _ -> walk 0 k)
walks 
|> Map.filter (fun k w -> k <> "shiny gold" && w |> Seq.map snd |> Seq.contains "shiny gold") 
|> Seq.length
|> Dump
//
//let rec walkCount num (next:color) =
//    let sub =
//        rules.[color]
//        |> List.collect (fun (n, color) -> walkCount ((int) n) color)
//    (num, color) :: sub

let sg = rules.["shiny gold"]

let rec totalBags (currentCount:int) nextColor =
    let sub =
        rules.[nextColor]
        |> List.map (fun (num, color) -> (int) num, color)
    
    match sub with 
    | [] -> 
        // printfn "End %i %s" currentCount nextColor
        let result = currentCount
        printfn "Terminal result for %s: %i" nextColor result
        result
    | _ -> 
        let result = 
            currentCount * (sub |> List.sumBy(fun (num, color) -> totalBags num color))
        printfn "Aggregate result for %s: %i" nextColor result
        result

// Should be 2431, not sure what the problem is ?
totalBags 1 "shiny gold" + (sg |> Seq.sumBy (fst >> int)) |> Dump
