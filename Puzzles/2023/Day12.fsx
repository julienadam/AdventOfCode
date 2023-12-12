
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
    |> Array.map parseLine


let countCombinations (springs, sections) =
    let rec countCombinationsRec damagedBefore (springs: SpringState list) (sections: int list) (acc:StringBuilder) =
        // printfn "dmg before %i. Line %A. Sections %A" damagedBefore springs sections
        match sections with
        | [] -> 
            // printfn "%s" (acc.ToString())
            1
        | section::remainingSections -> 
            match springs with
            | [] -> 0
            | spring::nextSprings ->
                match spring with
                | Operational when damagedBefore = section -> 
                    // Section is covered, continue on to the next section
                    countCombinationsRec 0 nextSprings remainingSections (acc.Append('.'))
                | Operational when damagedBefore = 0 -> 
                    // Spring works and we're not in a damaged set, move on
                    countCombinationsRec 0 nextSprings sections  (acc.Append('.'))
                | Operational -> 
                    0
                | Damaged when nextSprings = [] -> 
                    // Last spring, it's make or break !
                    if damagedBefore + 1 = section then 
                        countCombinationsRec 0 nextSprings remainingSections (acc.Append('#'))
                    else 
                        0
                | Damaged ->
                    // Continue on to the next spring in the same section
                    countCombinationsRec (damagedBefore + 1) nextSprings sections (acc.Append('#'))
                | Unknown -> 
                    // Try both variants
                    countCombinationsRec damagedBefore (Operational::nextSprings) sections (new StringBuilder(acc.ToString()))
                    + countCombinationsRec damagedBefore (Damaged::nextSprings) sections (new StringBuilder(acc.ToString()))
    
    countCombinationsRec 0 springs sections (new StringBuilder())

let testLine = parseLine >> countCombinations
Check.That(testLine "???.### 1,1,3").IsEqualTo(1)
Check.That(testLine ".??..??...?##. 1,1,3").IsEqualTo(4)
Check.That(testLine "?#?#?#?#?#?#?#? 1,3,1,6").IsEqualTo(1)
Check.That(testLine "????.#...#... 4,1,1").IsEqualTo(1)
Check.That(testLine "????.######..#####. 1,6,5").IsEqualTo(4)
Check.That(testLine "?###???????? 3,2,1").IsEqualTo(10)

let solve1 input =
    getInput input 
    |> Seq.sumBy countCombinations

solve1 "Day12.txt"
