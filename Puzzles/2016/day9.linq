<Query Kind="FSharpProgram" />

let toString : char seq -> string = Seq.map string >> String.concat ""
let getInputPath file = Path.Combine(Environment.GetFolderPath(Environment.SpecialFolder.UserProfile), @"CloudStation\Data\AdventOfCode\2016", file)
let path = getInputPath "day9.txt"
let inline mInt (name:string) (m:Match) = m.Groups.[name].Value |> int

let getRepetition input = 
    let m = Regex.Match(input, "(\d+)x(\d+)")
    if m.Success then
        (m |> mInt "1"), (m |> mInt "2")
    else
        failwithf "Not a valid repetition %s" input

let rec decompressRec index (output:StringBuilder) (input:string) =
    if index > input.Length - 1 then
        output.ToString()
    else
        match input.[index] with
        | '(' ->
            // This is a repetition pattern, extract it
            let closing = input.IndexOf(')', index)
            let (length, nb) = 
                input.Substring(index + 1, closing - index - 1)
                |> getRepetition
            
            // Repeat the pattern n times
            let pattern = input.Substring(closing + 1, length)
            for i = 1 to nb do
                output.Append(pattern) |> ignore
            decompressRec (closing + length + 1) output input 
        | ' ' ->
            // Ignore whitespace
            decompressRec (index + 1) output input 
        | c ->
            // Write normal chars directly
            decompressRec (index + 1) (output.Append(c)) input 
            
let decompress input = decompressRec 0 (new StringBuilder()) input

module Puzzle1 = 
    let tests() =
        if (decompress "ADVENT" |> Dump) <> "ADVENT" then failwithf "ADVENT failed"
        if (decompress "A(1x5)BC" |> Dump) <> "ABBBBBC" then failwithf "A(1x5)BC failed"
        if (decompress "(3x3)XYZ" |> Dump) <> "XYZXYZXYZ" then failwithf "(3x3)XYZ failed"
        if (decompress "A(2x2)BCD(2x2)EFG" |> Dump) <> "ABCBCDEFEFG" then failwithf "A(2x2)BCD(2x2)EFG failed"
        if (decompress "(6x1)(1x3)A" |> Dump) <> "(1x3)A" then failwithf "(6x1)(1x3)A failed"
        if (decompress "X(8x2)(3x3)ABCY" |> Dump) <> "X(3x3)ABC(3x3)ABCY" then failwithf "X(8x2)(3x3)ABCY failed"

    let solution () =
        let decompressed = 
            File.ReadAllText(path).Replace(" ", "") 
            |> decompress
            |> Dump
        printfn "Puzzle 1 : %i" decompressed.Length

// So you can't to it in memory ? Let's do it on disk !
// This is obviously crap, I would have gone for a recursive count if it didn't work
// But it worked so job done ! XD
// The last file is in the 10GB range, so not really a problem
module Puzzle2 =
    let rec readUntilRec (input:StreamReader) (c:char) (out:StringBuilder) =
        let read = input.Read() |> char
        if read = (0 |> char) then
            failwithf "Got to end of stream waiting for a %c" c
        else if read = c then
            out
        else 
            readUntilRec input c (out.Append(read))
        
    let readUntil input c = 
        let r = readUntilRec input c (new StringBuilder())
        r.ToString()
        
    let readN (input:StreamReader) n = 
        let (output:char[]) = Array.zeroCreate n
        input.ReadBlock(output, 0, n) |> ignore
        output |> toString
        
    let rec decompressToDiskRec (output:StreamWriter) (input:StreamReader) =
        if input.EndOfStream then
            ()
        else
            match input.Read() |> char with
            | '(' ->
                // This is a repetition pattern, extract it
                let (length, nb) = readUntil input ')' |> getRepetition
                // Repeat the pattern n times
                let pattern = readN input length
                for i = 1 to nb do
                    output.Write(pattern)
                    
                decompressToDiskRec output input 
            | ' ' ->
                // Ignore whitespace
                decompressToDiskRec output input 
            | c ->
                // Write normal chars directly
                output.Write(c)
                decompressToDiskRec output input 
           
    let hasParens path =
        use fs = File.OpenText(path)
        let mutable found = false
        let mutable result = false
        let parenInt = '(' |> int
        while not found do
            match fs.Read() with
            | 0 -> found <- true
            | i when i = parenInt -> 
                found <- true
                result <- true
            | _ -> ()
        result
    
    let decompressV2 (path:string) =
        let mutable iteration = 1
        let mutable input = path
        let mutable shouldContinue = true
        let mutable solution = 0L
        while shouldContinue do
            let outputFile = Path.Combine(@"E:\temp\", (sprintf "day9_iteration_%i.txt" iteration))
            use sw = File.CreateText(outputFile)
            use sr = File.OpenText(input)
            decompressToDiskRec sw sr
            sw.Flush()
            sw.Close()
            solution <- FileInfo(outputFile).Length
            shouldContinue = hasParens outputFile
            input <- outputFile
            iteration <- iteration + 1
        solution
       
    let solution() =
        decompressV2 (getInputPath "day9.txt") |> Dump

Puzzle2.solution()