#load "../../Tools.fs"

open System
open System.IO
open AdventOfCode

let path = getInputPath "day08.txt"
//let path = getInputPath "day08_sample2.txt"
//let path = getInputPath "day08_sample3.txt"
let debug = false

let inline dprintfn fmt =
    if debug then
        Printf.ksprintf System.Diagnostics.Debug.WriteLine fmt
    else
        ()

let mapLine (l:string) = 
    let split = l.Split("|")
    split.[0].Trim().Split(" "), split.[1].Trim().Split(" ")
    
let input = File.ReadLines(path) |> Seq.map mapLine |> Seq.toList

module Part1 =
    
    let Solve() = 
        input 
        |> List.map snd
        |> List.map (fun outputValues -> 
            outputValues 
                |> Array.filter (fun s -> 
                    match s.Length with 
                    | 2 -> true
                    | 4 -> true
                    | 3 -> true
                    | 7 -> true
                    | _ -> false) 
                |> Array.length)
        |> List.sum
        |> Dump
        ()
        
Part1.Solve()

module Part2 =
// 0:      1:      2:      3:      4:
//  aaaa    ....    aaaa    aaaa    ....
// b    c  .    c  .    c  .    c  b    c
// b    c  .    c  .    c  .    c  b    c
//  ....    ....    dddd    dddd    dddd
// e    f  .    f  e    .  .    f  .    f
// e    f  .    f  e    .  .    f  .    f
//  gggg    ....    gggg    gggg    ....
// 
//   5:      6:      7:      8:      9:
//  aaaa    aaaa    aaaa    aaaa    aaaa
// b    .  b    .  .    c  b    c  b    c
// b    .  b    .  .    c  b    c  b    c
//  dddd    dddd    ....    dddd    dddd
// .    f  e    f  .    f  e    f  .    f
// .    f  e    f  .    f  e    f  .    f
//  gggg    gggg    ....    gggg    gggg

    /// Takes the input display, runs it through the garbled segment -> normal segment mapping
    /// then finds the number displayed
    let getNumber input (mapping:Map<char, char>) =
            let translated = new String(input |> Seq.map (fun c -> mapping.[c]) |> Seq.sort |> Seq.toArray)
            match translated with
            | "abcefg" -> 0
            | "cf" -> 1
            | "acdeg" -> 2
            | "acdfg" -> 3
            | "bcdf" -> 4
            | "abdfg" -> 5
            | "abdefg" -> 6
            | "acf" -> 7
            | "abcdefg" -> 8
            | "abcdfg" -> 9
            | _ -> failwithf "invalid display %s, input was %s" translated input

    /// Main method, using a pair of garbled display input, applies some logic to reduce the possible mappings
    /// between normal segment identifier and garbled segment identifier 
    let updateSegmentCandidates (display1:string) (display2:string) (segmentCandidates:Map<char, char Set>)=
        let set1 = (display1 |> Set.ofSeq)
        let set2 = (display2 |> Set.ofSeq)
    
        // Compare the length of both displays
        match display1.Length, display2.Length with
        | 2, 3 -> 
            dprintfn "Updating 2 & 3"
            // Diff between 7 (3 segments) and 1 (2 segments) gives us the 'a' segment directly
            // 'c' and 'f' are one of the 2 segments used in display1
            let x = Set.difference set2 set1
            if x |> Set.count <> 1 then failwithf "Should not happen"
            segmentCandidates
                .Add('a', x)
                .Add('c', Set.intersect set1 segmentCandidates.['c'])
                .Add('f', Set.intersect set1 segmentCandidates.['f'])
        | 2, 4 -> 
            dprintfn "Updating 2 & 4"
            // Diff between 4 (4 segments) and 1 (2 segments) gives us the 'b' or 'd' segments
            // 'c' and 'f' are still of the 2 segments used in display1
            let x = Set.difference set2 set1
            if x |> Set.count <> 2 then failwithf "Should not happen"
            segmentCandidates
                .Add('b', Set.intersect x segmentCandidates.['b'])
                .Add('d', Set.intersect x segmentCandidates.['d'])
                .Add('c', Set.intersect set1 segmentCandidates.['c'])
                .Add('f', Set.intersect set1 segmentCandidates.['f'])
        | 2, 5 -> 
            // Diff between 2,3 or 5 (5 segments) and 1 (2 segments) is only interesting if it's 3 segments in that case display2 is a 3 and we can update accordingly
            let x = Set.difference set2 set1
            match x |> Set.count with
            | 3 -> 
                dprintfn "Updating 2 & 5, option 3"
                // TODO:  We know display2 is a 3, store that for later ?
                // 'a' 'd' 'g' are 
                segmentCandidates
                    .Add('a', Set.intersect x segmentCandidates.['a'])
                    .Add('d', Set.intersect x segmentCandidates.['d'])
                    .Add('g', Set.intersect x segmentCandidates.['g'])
                    // 'c' and 'f' are still of the 2 segments used in display1
                    .Add('c', Set.intersect set1 segmentCandidates.['c'])
                    .Add('f', Set.intersect set1 segmentCandidates.['f'])
            | 4 -> 
                dprintfn "Updating 2 & 5, option 2 or 5"
                segmentCandidates
                    .Add('c', Set.intersect set1 segmentCandidates.['c'])
                    .Add('f', Set.intersect set1 segmentCandidates.['f'])
            | _ -> failwith "Invalid XOR result, case (2,5)"
        | 2, 6 ->
            // Diff between 0, 6 or 9 (6 segments) and 1 (2 segments) is only interesting if it's 5 segments in that case display2 is a 6 and we can update accordingly
            let x = Set.difference set2 set1
            match x |> Set.count with
            | 5 -> 
                dprintfn "Updating 2 & 6, option 6"
                // display2 is a 6, we can determine both f and c from that directly
                let f = Set.intersect set1 set2
                if f |> Set.count <> 1 then failwithf "Should not happen"
                let c = Set.difference set1 f
                if c |> Set.count <> 1 then failwithf "Should not happen"
                segmentCandidates
                    .Add('f', f)
                    .Add('c', c)
            | 4 ->
                dprintfn "Updating 2 & 6, option 0 or 9"
                segmentCandidates
                    .Add('c', Set.intersect set1 segmentCandidates.['c'])
                    .Add('f', Set.intersect set1 segmentCandidates.['f'])
            | _ -> failwith "Invalid XOR result, case (2,6)"
        // NOTE: There may be other deductions to make below but I found the solution without further logic
        | 3, 4 -> segmentCandidates
        | 3, 5 -> segmentCandidates
        | 3, 6 -> segmentCandidates
        | 4, 5 -> segmentCandidates
        | 4, 6 -> segmentCandidates
        | 5, 5 -> segmentCandidates
        | 5, 6 -> segmentCandidates
        | 6, 6 -> segmentCandidates
        | 7, _ -> segmentCandidates
        | _, 7 -> segmentCandidates
        | x, y -> failwithf "invalid number combination %i %i" x y 
        
    /// Finds all garbled segment ids that have been mapped
    let findFixedSegments (candidates:Map<char, char Set>) =
        candidates 
        |> Seq.map (fun kvp -> kvp.Value)
        |> Seq.filter (fun cands -> cands |> Set.count = 1)
        |> Seq.collect id
        
    /// Verifies a few invariant to help track down bugs
    let verifyInvariants (candidates:Map<char, char Set>) =
        if candidates |> Map.exists (fun _ cands -> cands |> Set.count > 7) then
            failwithf "More than 7 candidates found"
        
        let fixedSegments = candidates |> findFixedSegments
        if fixedSegments |> Seq.groupBy id |> Seq.exists (fun (_, chars) -> (chars |> Seq.length) > 1) then
            failwithf "Same single candidate found for different segments"
        
    /// Remove garbled segment identifiers that have been identified from the
    /// other possible mappings. Function is recursive because 
    /// after reducing a first time, other garbled segment can be identified
    let rec reduceCandidates (candidates:Map<char, char Set>) =
        // Find segments that are fixed, i.e. only one candidate
        let fixedOnes = candidates |> findFixedSegments |> Set.ofSeq
        if debug then
            fixedOnes |> Dump |> ignore
        
        let mutable changed = false
        // Remove those candidates from other segments
        let result = 
            candidates |> Map.map (fun _ cands -> 
                if cands |> Set.count > 1 then
                    let diff = Set.difference cands fixedOnes
                    if (diff |> Set.count) <> (cands |> Set.count) then
                        changed <- true
                    diff
                else
                    cands
            )
        
        // If a change was made, try again until no change needs to be applied
        if changed then
            reduceCandidates result
        else
            result
    
    /// Main solving method
    let rec SolveRec (displayCouples:(string * string) list) (candidates:Map<char, char Set>) = 
        verifyInvariants candidates
        let reducedCandidates = candidates |> reduceCandidates
        if debug then
            reducedCandidates |> Dump |> ignore
    
        if reducedCandidates |> Map.exists (fun _ candidateSet -> candidateSet.Count = 0) then
            // Negative exit condition, no candidate left for at least one segment
            None
        else if reducedCandidates |> Map.forall (fun _ candidateSet -> candidateSet.Count = 1) then
            // Positive exit condition, each segment has a single candidate
            Some reducedCandidates
        else 
            match displayCouples with
            | [] -> None
            | (d1, d2) :: rest ->
                SolveRec rest (reducedCandidates |> updateSegmentCandidates d1 d2)
    
    let Solve() = 
        /// Build the starting mapping possibilities
        let fullSet = "abcdefg" |> Set.ofSeq
        let segmentCandidates = fullSet |> Seq.map (fun c -> c, fullSet) |> Map.ofSeq
            
        let sw = Stopwatch.StartNew()
        input 
            |> Seq.map (fun (left, right) ->
                // Take the left displays and build a list of couples
                // excluding duplicates
                let couples = 
                    (Seq.allPairs left left) 
                    |> Seq.filter (fun (s1,s2) -> (s1 <> s2) && (s1.Length <= s2.Length)) 
                    |> Seq.toList
                    // |> Dump
                
                // Apply the mapping deducing logic, then reverse the mapping to get a garbled -> normal mapping
                let mapping = 
                    match SolveRec couples segmentCandidates with
                    | Some reverseMapping -> 
                        reverseMapping |> Seq.map (fun kvp -> kvp.Value |> Seq.head, kvp.Key) |> Map.ofSeq
                    | None -> failwithf "no mapping found for \r\n%s | %s" (String.Join(" ", left)) (String.Join(" ", right))
                
                // Using the mapping, extract the numbers for the right side display
                let dec = right |> Array.map (fun input -> getNumber input mapping)
                // And build an int from that
                dec.[0] * 1000 + dec.[1] * 100 + dec.[2] * 10 + dec.[3]
                )
            |> Seq.sum
            |> Dump
        printfn "%A" sw.Elapsed
        
        ()

    let SolveV2() =
        // Every line contains the whole 0-9 range, from that we can use some deducing logic
        // without having to resort to segment mapping
        let reorder (s:seq<char>) = new String(s |> Seq.sort |> Seq.toArray)
        
        let getDisplayToNumbersMap (input:string[]) =
            
            // find 1 4 7 8
            let d1 = input |> Seq.filter (fun s -> s.Length = 2) |> Seq.exactlyOne
            let d4 = input |> Seq.filter (fun s -> s.Length = 4) |> Seq.exactlyOne
            let d7 = input |> Seq.filter (fun s -> s.Length = 3) |> Seq.exactlyOne
            let d8 = input |> Seq.filter (fun s -> s.Length = 7) |> Seq.exactlyOne
            
            // with 4 & (2,5,3) find 2
            let d4Set = d4 |> Set.ofSeq
            let fiveSegments = input |> Array.filter (fun s -> s.Length = 5)
            let fiveSegmentsAndD4 = fiveSegments |> Seq.sortBy (fun a -> (Set.intersect (a |> Set.ofSeq) d4Set) |> Set.count)
            let d2 = fiveSegmentsAndD4 |> Seq.head
            
            // with 1 & (5,3) find 3 = 2segs the other is 5
            let d1Set = d1 |> Set.ofSeq
            let threeOrFive = fiveSegments |> Seq.filter (fun s -> s <> d2)
            
            let d3 = threeOrFive |> Seq.filter (fun a -> (Set.intersect (a |> Set.ofSeq) d1Set) |> Set.count = 2) |> Seq.exactlyOne
            let d5 = threeOrFive |> Seq.filter (fun s -> s <> d3) |> Seq.exactlyOne
            
            // with 4 & (6,9,0) = 4segs find 9
            // with 1 & (6,9,0) = 1segs find 6 the other is 0. Problem solved
            let sixOrNineOrZero = input |> Array.filter (fun s -> s.Length = 6)
            let d9 = sixOrNineOrZero |> Seq.filter (fun a -> (Set.intersect (a |> Set.ofSeq) d4Set) |> Set.count = 4) |> Seq.exactlyOne
            let d6 = sixOrNineOrZero |> Seq.filter (fun a -> (Set.intersect (a |> Set.ofSeq) d1Set) |> Set.count = 1) |> Seq.exactlyOne
            let d0 = sixOrNineOrZero |> Seq.filter (fun s -> s <> d6 && s <> d9) |> Seq.exactlyOne
            
            [
                (d0 |> reorder, 0); 
                (d1 |> reorder, 1); 
                (d2 |> reorder, 2); 
                (d3 |> reorder, 3); 
                (d4 |> reorder, 4); 
                (d5 |> reorder, 5); 
                (d6 |> reorder, 6); 
                (d7 |> reorder, 7); 
                (d8 |> reorder, 8); 
                (d9 |> reorder, 9)
            ] |> Map.ofSeq
            
        input 
        //|> Seq.take 1
        |> Seq.map (fun (left, right) ->
            let mapping = getDisplayToNumbersMap left
            let numbers = right |> Array.map (fun s -> 
                let reordered = s |> reorder
                match mapping |> Map.tryFind reordered with
                | Some x -> x
                | None -> failwithf "No mapping found for %s (initially %s)" reordered s)
            1000 * numbers.[0] + 100 * numbers.[1] + 10 * numbers.[2] + numbers.[3])
        |> Seq.sum
        |> Dump
        ()
            
            
Part2.SolveV2()


