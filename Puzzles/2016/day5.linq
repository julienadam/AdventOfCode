<Query Kind="FSharpProgram" />

let toString : char seq -> string = Seq.map string >> String.concat ""

let md5 (input:string) =
    let bytes = System.Text.Encoding.ASCII.GetBytes(input)
    System.Security.Cryptography.MD5.HashData(bytes)
    
let toHexString (bytes:byte[]) =
    let sb = new StringBuilder(bytes.Length * 2)
    bytes |> Seq.iter (fun b -> sb.Append(b.ToString("x2")) |> ignore)
    sb.ToString()

let findNextInterestingHash input start =
    Seq.initInfinite(fun i -> start + i)
    |> Seq.map(fun i -> 
        let currentCandidate = (sprintf "%s%i" input i)
        i, currentCandidate |> md5)
    |> Seq.pick(fun (i, hash) -> 
       // First 2 bytes are 0, third byte starts with 0000 (hence, not 1111 0000 or not 240uy)
       if hash.[0] = 0uy && hash.[1] = 0uy && (hash.[2] &&& 240uy = 0uy) then
            Some (i, toHexString hash)
        else
            None)
    
module Puzzle1 =
    let findPassword input = 
        [1..8] |> Seq.fold (fun (start, pwd) _ ->
            let (nextStart, interestingHash) = findNextInterestingHash input (start + 1)
            let nextChar = interestingHash.[5]
            (nextStart, pwd @ [nextChar])) (0, [])
            
    let solution() =    
        // findPassword "abc" |> snd |> toString |> Dump
        findPassword "reyedfim" |> snd |> toString |> Dump
    
    
//Puzzle1.solution()

module Puzzle2 = 
    
    let rec findPassword input start pwd =
        if pwd |> List.length = 8 then
            pwd
        else
            let (nextStart, interestingHash) = findNextInterestingHash input (start+1)
            if interestingHash.[5] >= '0' && interestingHash.[5] <= '7' then
                let pos = (interestingHash.[5] |> int) - ('0' |> int)
                if pwd |> List.exists (fun (p, _) -> p = pos) then
                    // A character was already found at that position, continue
                    findPassword input nextStart pwd
                else
                    // Found a valid password character
                    let pwdChar = pos, interestingHash.[6]
                    findPassword input nextStart (pwdChar :: pwd)
            else
                // Continue searching
                findPassword input nextStart pwd

    let solution() =
        findPassword "reyedfim" 0 [] 
        |> Seq.sortBy fst
        |> Seq.map snd
        |> toString
        |> Dump
        
Puzzle2.solution()