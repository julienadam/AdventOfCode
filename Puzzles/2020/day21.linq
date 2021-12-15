<Query Kind="FSharpProgram" />

let getInputPath file = Path.Combine(Environment.GetFolderPath(Environment.SpecialFolder.UserProfile), @"CloudStation\Data\AdventOfCode\", file)
let path = getInputPath "day21.txt"

let parseLine line =
    let m = Regex.Match(line, "(?<ingredients>.*) \(contains (?<allergens>.*)\)") 
    m.Groups.["ingredients"].Value.Split(" "), m.Groups.["allergens"].Value.Split(", ")

let input = 
    File.ReadAllLines path
    |> Array.map parseLine


// for a given food item, each allergen corresponds to one ingredient in the list
// allergen = exactly 1 ingredient
// ingredient = 0 or 1 allergen

// which ingredients can't possibly contain any of the allergens in any food in your list

// mxmxvkd kfcds sqjhc nhms (contains dairy, fish)
// trh fvjkl sbzzf mxmxvkd (contains dairy)
// sqjhc fvjkl (contains soy)
// sqjhc mxmxvkd sbzzf (contains fish)

// kfcds, nhms, sbzzf, trh 
// Counting the number of times any of these ingredients appear in any ingredients list produces 5
// they all appear once each except sbzzf, which appears twice.

let getFoodIngredientsWithAllergen allergen input =
    input 
    |> Seq.filter (fun (_, allergens) -> allergens |> Array.contains allergen)
    |> Seq.map fst
    |> Seq.map Set.ofArray

let allergens = 
    input 
    |> Seq.collect snd 
    |> Set.ofSeq 

let ingredients = 
    input 
    |> Seq.collect fst 
    |> Set.ofSeq 

let possibleIngredientsByAllergen =
    allergens 
    |> Seq.map (fun allergen -> allergen, (getFoodIngredientsWithAllergen allergen input) |> Set.intersectMany)
    
let potentiallyAllergenicIngredients =
    possibleIngredientsByAllergen 
    |> Seq.collect snd 
    |> Set.ofSeq 

let nonAllergenicIngredients = 
    Set.difference ingredients potentiallyAllergenicIngredients 
    
module Puzzle1 =
    let solution() =
        printfn "Nombre de fois qu'un ingrédient non allergénique apparait dans la liste d'aliments"
        input 
        |> Seq.collect fst 
        |> Seq.filter (fun ingredient -> nonAllergenicIngredients |> Set.contains ingredient)
        |> Seq.length
        |> Dump
        
module Puzzle2 =
    let solution() =
        let filteredInput = 
            input 
            |> Seq.map (fun (ingredients, allergens) ->
                ingredients 
                |> Array.where (fun i -> potentiallyAllergenicIngredients.Contains i), allergens)
        
        let ingredientsByAllergen = 
            allergens 
            |> Seq.map (fun allergen -> 
                allergen, (getFoodIngredientsWithAllergen allergen filteredInput) |> Set.intersectMany)
            |> Seq.toList
        
        let dic = new Dictionary<string, string>()
        
        let rec assignIngredients (ingredientsByAllergen: (string * Set<string>) list) =
            match ingredientsByAllergen with
            | [] -> ()
            | l ->
                l
                |> Seq.filter (fun (_, ings) -> ings.Count = 1)
                |> Seq.iter (fun (a, ings) -> dic.Add(ings |> Seq.head, a))
            
                let remaining = 
                    l
                    |> List.map (fun (a, ings) ->
                        (a, ings |> Set.filter (fun i -> dic.ContainsKey(i) |> not)))
                    |> List.where (fun (_, ings) -> ings.Count > 0)
                
                assignIngredients remaining
            
        assignIngredients ingredientsByAllergen
                
        let canonicalDangerousIngredientList = 
            dic 
            |> Seq.map (fun kvp -> kvp.Value, kvp.Key)
            |> Seq.sortBy fst
            |> Seq.map snd
            |> Seq.toArray
        
        String.Join(",", canonicalDangerousIngredientList) |> Dump

Puzzle2.solution() 