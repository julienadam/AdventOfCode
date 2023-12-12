
#time
#load "../../Tools.fs"
#r "nuget: NFluent"

open System
open System.IO
open AdventOfCode
open System.Text
open NFluent

type SpringState =
    | Operational
    | Damaged
    | Unknown

let charToState = function | '?' -> Unknown | '.' -> Operational | '#' -> Damaged | _ -> failwithf "Invalid state"
let parseLine line =
        let springStates, damageReport = line |> ssplit2 " "
        springStates |> Seq.map charToState |> List.ofSeq, damageReport |> splitIntList |> Array.toList

let getInput name = 
    File.ReadAllLines(getInputPath2023 name)
    // |> Array.map parseLine


let verifySolution (springs:SpringState list) (solution:string) sections =
    if solution.Contains("?") then failwithf "Should not contain unknowns"
    let actualSections = 
        solution.Split(".", StringSplitOptions.RemoveEmptyEntries) 
        |> Seq.map (fun g -> g.Length) 
        |> Seq.toList
    if actualSections <> sections then
        failwithf "Actual sections %A different from computed sections %A" actualSections sections
    if solution.Length <> springs.Length then
        failwithf "Solution %s does not have the same length as the initial line" solution
    Seq.zip springs solution
    |> Seq.iter (fun (s, c) ->
        match (s,c) with
        | Damaged, '#' -> ()
        | Operational, '.' -> ()
        | Unknown, '.' -> ()
        | Unknown, '#' -> ()
        | _ -> failwithf "Invalid match, state was %O but replaced with %c" s c
    )

let countCombinations (initialSprings, initialSections) =
    let rec countCombinationsRec damagedBefore (springs: SpringState list) (sections: int list) (acc:StringBuilder) =
        match sections, springs with
        | [], [] -> 
                let solution = acc.ToString()
                // printfn "%s" solution
                verifySolution initialSprings solution initialSections
                1
        | [], spring::nextSprings ->
            match spring with
            | Unknown
            | Operational -> 
                // Move on with an operational spring since a damaged one would fail
                countCombinationsRec 0 nextSprings [] (acc.Append('.'))
            | Damaged ->
                // We have a damaged spring but no more sections left !
                0
        | _, [] -> 0
        | section::remainingSections, Operational::nextSprings when damagedBefore = section ->
            // Section is covered, continue on to the next section
            countCombinationsRec 0 nextSprings remainingSections (acc.Append('.'))
        | _, Operational::nextSprings when damagedBefore = 0 ->
            // Spring works and we're not in a damaged set, move on
            countCombinationsRec 0 nextSprings sections  (acc.Append('.'))
        | _, Operational::_ -> 0
        | section::remainingSections, [Damaged] ->
            // Last spring, it's make or break !
            if damagedBefore + 1 = section then 
                countCombinationsRec 0 [] remainingSections (acc.Append('#'))
            else 
                0
        | _, Damaged::nextSprings ->
            // Continue on to the next spring in the same section
            countCombinationsRec (damagedBefore + 1) nextSprings sections (acc.Append('#'))
        | _, Unknown::nextSprings ->
            // Try both variants
            countCombinationsRec damagedBefore (Operational::nextSprings) sections (new StringBuilder(acc.ToString()))
            + countCombinationsRec damagedBefore (Damaged::nextSprings) sections (new StringBuilder(acc.ToString()))
      
    countCombinationsRec 0 initialSprings initialSections (new StringBuilder())

let processLine = parseLine >> countCombinations
Check.That(processLine "???.### 1,1,3").IsEqualTo(1)
Check.That(processLine ".??..??...?##. 1,1,3").IsEqualTo(4)
Check.That(processLine "?#?#?#?#?#?#?#? 1,3,1,6").IsEqualTo(1)
Check.That(processLine "????.#...#... 4,1,1").IsEqualTo(1)
Check.That(processLine "????.######..#####. 1,6,5").IsEqualTo(4)
Check.That(processLine "?###???????? 3,2,1").IsEqualTo(10)

let solve1 input =
    getInput input 
    |> Seq.sumBy processLine

Check.That(solve1 "Day12_sample.txt").IsEqualTo(21)

solve1 "Day12.txt"
