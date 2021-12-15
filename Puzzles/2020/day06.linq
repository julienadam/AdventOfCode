<Query Kind="FSharpProgram" />

let path = 
    Path.Combine(
        Environment.GetFolderPath(Environment.SpecialFolder.UserProfile), 
        "CloudStation\Data\AdventOfCode\day6.txt")
let input:System.String = File.ReadAllText(path)

let splitLines predicate xs =
        List.foldBack (fun x acc ->
            match acc with 
            | []     -> failwith "unexpected"
            | hd::tl ->
                if predicate x
                then ([]::(hd::tl))
                else ((x::hd)::tl)) (Seq.toList xs) [[]]
            
let groups = input.Split("\r\n\r\n")

let s1 () =
    groups 
    |> Seq.map(fun s -> 
        s.Split("\r\n")
        |> Array.collect (fun s -> s.ToCharArray())
        |> Seq.distinct
        |> Seq.length) 
    |> Seq.sum
    |> Dump
    
s1()
    
let s2 () =
    groups
    |> Seq.map(fun groupLines ->
        groupLines.Split("\r\n")
        |> Array.map Set
        |> Set.intersectMany
        |> Set.count)
    |> Seq.sum

printfn "S2"
s2 () |> Dump

//
//let s3 () =
//    
//    File.ReadAllLines path 
//    |> splitLines ((=) "")
//    |> Seq.map ( fun s ->
//                  String.Join("\r\n", s), s |> (List.map Set
//                  >> Seq.reduce Set.intersect
//                  >> Seq.distinct
//                  >> Set
//                  // >> Seq.length))
//                  ))
//
//printfn "S3"
//s3 () |> Dump
//
//// mine |> Seq.zip theirs |> Seq.where (fun (m,t) -> (snd m) <> (snd t)) |> Dump