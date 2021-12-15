<Query Kind="FSharpProgram">
  <NuGetReference>FSharp.Collections.ParallelSeq</NuGetReference>
  <Namespace>FSharp.Collections.ParallelSeq</Namespace>
</Query>

let path = 
    Path.Combine(
        Environment.GetFolderPath(Environment.SpecialFolder.UserProfile), 
        "CloudStation\Data\AdventOfCode\day13.txt")
let input = File.ReadAllLines(path)
let timestamp = input.[0] |> int
let stringToBus s = match s with | "x" -> None | id -> Some (id |> int)
let buses = input.[1].Split(',') |> Seq.choose stringToBus


module Puzzle1 =
    let findNextDeparture timestamp id  = 
        id, ((timestamp / id) + 1) * id
    
    let solution () =
        let id, tsAfterWait = 
            buses |> Seq.map (findNextDeparture timestamp) |> Seq.minBy snd |> Dump

        (tsAfterWait - timestamp) * id
        
// Puzzle1.solution() |> Dump |> ignore

let infiniteULongMultiplesOf mult = seq {
        let mutable current = mult // 100000000000000UL / mult * mult
        while true do
            yield current
            current <- current + mult
    }


let infiniteULongMultiplesOfWithOffset offSet mult = seq {
        let mutable current = mult - offSet// 100000000000000UL / mult * mult
        while true do
            yield current
            current <- current + mult
    }
    

let infiniteULongMultiplesOfWithPositiveOffset offSet mult = seq {
        let mutable current = offSet// 100000000000000UL / mult * mult
        while true do
            yield current
            current <- current + mult
    }

module Puzzle2 =
    let busDifferences = 
        input.[1].Split(',') 
        |> Seq.mapi (fun offset s -> offset, stringToBus s) 
        |> Seq.choose (fun (offset, b) -> match b with | Some id -> Some (offset |> uint64, id |> uint64) | _ -> None)
        |> Seq.toList
        
    let solution() = 
        let (referenceOffset, maxBusId) = busDifferences |> Seq.maxBy snd;
        let checkBusDiffs = busDifferences |> Seq.where (fun (_,id) -> id <> maxBusId) |> Seq.sortByDescending snd |> Seq.toList
        
        let firstMatch =
            infiniteULongMultiplesOf maxBusId
            |> Seq.find (fun i -> 
                checkBusDiffs
                |> Seq.exists(fun (offset, id) -> 
                    ((i - referenceOffset + offset) % id) <> 0UL)
                |> not)
                
        firstMatch - referenceOffset

module Puzzle2b =
    let busDifferences = 
        input.[1].Split(',') 
        |> Seq.mapi (fun offset s -> offset, stringToBus s) 
        |> Seq.choose (fun (offset, b) -> match b with | Some id -> Some (offset |> uint64, id |> uint64) | _ -> None)
        |> Seq.toList
        
    let findSolutions candidateSource (offset, id) =
        candidateSource
        |> Seq.where (fun i -> (i + offset) % id = 0UL)
        
    let solution() = 
        let diffSorted = busDifferences |> Seq.sortByDescending snd
        let (offset, topId) = diffSorted |> Seq.head
        
        let topmult = infiniteULongMultiplesOfWithOffset offset topId |> Seq.toList
        
        let chain = diffSorted |> Seq.fold (fun state (offset, id) -> findSolutions state (offset, id)) (infiniteULongMultiplesOfWithOffset offset topId)
        
        chain |> Seq.head
  
module Puzzle2c =
    
    let busDifferences = 
        input.[1].Split(',') 
        |> Seq.mapi (fun offset s -> offset, stringToBus s) 
        |> Seq.choose (fun (offset, b) -> match b with | Some id -> Some (offset |> uint64, id |> uint64) | _ -> None)
        |> Seq.toList
        |> Seq.sortByDescending snd
        |> Seq.toList
        
    let solution () =
        
        let folder (numberSequence, mult) (offset, id) =
            let expected = if offset = 0UL then 0UL else id - offset
            let found = numberSequence |> Seq.find (fun x -> x % id = expected)
            printfn "Found %i, x === %i mod %i" found expected id
            (infiniteULongMultiplesOfWithPositiveOffset found (mult*id), mult*id)
        
        // let (offset, id) = busDifferences |> Seq.head
        
        let (results, _) = 
            busDifferences |> Seq.fold folder ((infiniteULongMultiplesOf 1UL), 1UL)
        
        results |> Seq.head |> Dump
        
        //let s1 = 
        //    infiniteULongMultiplesOfWithPositiveOffset 4UL 5UL 
        //    |> Seq.find (fun i -> i % 4UL = 3UL)
        //
        //infiniteULongMultiplesOfWithPositiveOffset s1 (4UL * 5UL) 
        //|> Seq.find (fun i -> i % 3UL = 0UL) 
        //|> Dump

module Puzzle2d =
    let busDifferences = 
        input.[1].Split(',') 
        |> Seq.mapi (fun offset s -> offset, stringToBus s) 
        |> Seq.choose (fun (offset, b) -> match b with | Some id -> Some (offset |> int64, id |> int64) | _ -> None)
        |> Seq.toList
        |> Seq.sortByDescending snd
        |> Seq.toList
        
    // Modular inverse
    let MI n g =
        let rec fN n i g e l a =
            match e with
            | 0L -> g
            | _ -> let o = n/e
                   fN e l a (n-o*e) (i-o*l) (g-o*a) 
        (n+(fN n 1L 0L g 0L 1L))%n

    // Greater common denominator
    let rec gcd a b =
        if b = 0L 
            then abs a
        else gcd b (a % b)
  
    // Chinese remainder
    let CD n g =
        match Seq.fold(fun n g->if (gcd n g)=1L then n*g else 0L) 1L g with
        |0L -> None
        |fN -> Some ((Seq.fold2(fun n i g -> n+i*(fN/g)*(MI g ((fN/g)%g))) 0L n g)%fN)
          
    let solution () =
        let offsets = (busDifferences |> Seq.map snd) |> Seq.toList
        let ids = (busDifferences |> Seq.map (fun (offset, id) -> if offset = 0L then 0L else id - offset)) |> Seq.toList
        CD ids offsets

let sw = Stopwatch.StartNew()
let s = Puzzle2d.solution()
printfn "found %s in %s" (s.ToString()) (sw.Elapsed.ToString())
