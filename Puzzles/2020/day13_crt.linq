<Query Kind="FSharpProgram" />

let path = 
    Path.Combine(
        Environment.GetFolderPath(Environment.SpecialFolder.UserProfile), 
        "CloudStation\Data\AdventOfCode\day13.txt")
let input = File.ReadAllLines(path)
let timestamp = input.[0] |> int
let stringToBus s = match s with | "x" -> None | id -> Some (id |> int)
let buses = input.[1].Split(',') |> Seq.choose stringToBus

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

module Puzzle2d =
    
    let busDifferences = 
        input.[1].Split(',') 
        |> Seq.mapi (fun offset s -> offset, stringToBus s) 
        |> Seq.choose (fun (offset, b) -> match b with | Some id -> Some (offset |> int64, id |> int64) | _ -> None)
        |> Seq.toList
        |> Seq.sortByDescending snd
        |> Seq.toList

    let MI n g =
        let rec fN n i g e l a =
            match e with
            | 0L -> g
            | _ -> let o = n/e
                   fN e l a (n-o*e) (i-o*l) (g-o*a) 
        (n+(fN n 1L 0L g 0L 1L))%n

    let rec gcd a b =
        if b = 0L 
            then abs a
        else gcd b (a % b)
  
    let CD n g =
        match Seq.fold(fun n g->if (gcd n g)=1L then n*g else 0L) 1L g with
        |0L -> None
        |fN -> Some ((Seq.fold2(fun n i g -> n+i*(fN/g)*(MI g ((fN/g)%g))) 0L n g)%fN)
          
    let solution () =
        let offsets = (busDifferences |> Seq.map snd) |> Seq.toList
        let ids = (busDifferences |> Seq.map (fun (offset, id) -> if offset = 0L then 0L else id - offset)) |> Seq.toList
        CD ids offsets
         
let sw = Stopwatch.StartNew()
match Puzzle2d.solution() with
| Some s -> printfn "found %s in %s" (s.ToString()) (sw.Elapsed.ToString())
| None -> printfn "No solution found"
