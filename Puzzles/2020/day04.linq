<Query Kind="FSharpProgram" />

let path = Path.Combine(Environment.GetFolderPath(Environment.SpecialFolder.UserProfile), "CloudStation\Data\AdventOfCode\day4.txt")
let input:System.String = File.ReadAllText(path)

let passports = input.Split([|"\r\n\r\n"|], StringSplitOptions.None)

let r = new Regex("(\w\w\w):([^\s]+)")

let readData input = seq {
        for m in r.Matches(input) do
            yield m.Groups.[1].Value, m.Groups.[2].Value
    }
    
let s1 () =
    let isValid (data:Map<string, string>) =
        ["byr"; "iyr"; "eyr"; "hgt"; "hcl"; "ecl"; "pid"]
        |> Seq.map (fun field -> data |> Map.containsKey field)
        |> Seq.fold (fun a b -> a && b)  true
            
    passports 
    |> Seq.map (fun input -> 
        input 
        |> readData 
        |> Map.ofSeq)
    |> Seq.map isValid
    |> Seq.where id
    |> Seq.length
    
// s1 () |> Dump

let s2 () =

    let isYear min max input = 
        match Int32.TryParse input with
        | true, i -> i >= min && i <= max
        | _ -> false

    let validHeight input =
        let r = new Regex("^(\d+)(cm|in)$")
        match r.Match(input) with
        | m when m.Success ->
            let h = m.Groups.[1].Value |> int
            match m.Groups.[2].Value with
            | "cm" -> h >= 150 && h <= 193
            | "in" -> h >= 59 && h <= 76
            | s -> failwithf "Unknown height unit %s" s
        | _ -> false

    let validHairColor input =
        let r = new Regex("^#[0-9a-f]{6}$")
        r.IsMatch(input)

    let validEyeColor input =
        ["amb";"blu";"brn";"gry"; "grn"; "hzl"; "oth"].Contains(input)

    let validPassportId input =
        let r = new Regex("^[0-9]{9}$")
        r.IsMatch(input)
        
    let isValid (data:Map<string, string>) =
        [
            "byr", isYear 1920 2002
            "iyr", isYear 2010 2020
            "eyr", isYear 2020 2030
            "hgt", validHeight
            "hcl", validHairColor
            "ecl", validEyeColor
            "pid", validPassportId
        ]
        |> Seq.map (fun (field, validator) -> 
            match data |> Map.tryFind field with
            | Some d -> validator(d)
            | None -> false)
        |> Seq.fold (fun a b -> a && b)  true
            
    passports 
    |> Seq.map (fun input -> 
        input 
        |> readData 
        |> Map.ofSeq)
    |> Seq.map isValid
    |> Seq.where id
    |> Seq.length
    
s2 () |> Dump