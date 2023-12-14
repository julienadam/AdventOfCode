namespace AdventOfCode

open System
open System.IO
open System.Collections.Generic

[<AutoOpen>]
module Tools =

    let getInputPath file = Path.Combine(__SOURCE_DIRECTORY__, "Input", "2021", file)
    let getInputPath2022 file = Path.Combine(__SOURCE_DIRECTORY__, "Input", "2022", file)
    let getInputPath2023 file = Path.Combine(__SOURCE_DIRECTORY__, "Input", "2023", file)
    let getInputPathFsx (sourceDir:string) (sourceFile:string) qualifier =
        let year = DirectoryInfo(sourceDir).Name
        let fileName = sprintf "%s%s.txt" (Path.GetFileNameWithoutExtension(sourceFile)) qualifier
        Path.Combine(__SOURCE_DIRECTORY__, "Input", year, fileName)

    let Dump obj =
        printfn "%A" obj
        obj

    let ssplit (sep:string) (str:string) = str.Split([|sep|], StringSplitOptions.None)

    let ssplitNoEmpty (sep:string) (str:string) = str.Split([|sep|], StringSplitOptions.RemoveEmptyEntries)

    let ssplit2 (sep : string) (s : string) = 
        let split = s.Split(sep)
        split.[0], split.[1]

    let split2 (sep : char) (s : string) = 
        let split = s.Split(sep)
        split.[0], split.[1]

    let splitIntList (input:string) = 
        input.Split([|","|], StringSplitOptions.RemoveEmptyEntries) |> Array.map (fun s -> Int32.Parse s)

    let splitSpaceIntList (input:string) = 
        input.Split([|" "|], StringSplitOptions.RemoveEmptyEntries) |> Array.map (fun s -> Int32.Parse s)

    let splitSpaceIntList64 (input:string) = 
        input.Split([|" "|], StringSplitOptions.RemoveEmptyEntries) |> Array.map (fun s -> Int64.Parse s)

    let splitLines input = seq {
        use sr = new StringReader(input)
        let mutable s = ""
        while (s <> null) do
            s <- sr.ReadLine()
            if s <> null then
                yield s
    }

    let inline ltupleize2 (l:list<'a>) =
        match l with
        | [a;b] -> a,b
        | _ -> failwithf "Not a list of 2 elements"

    let inline tupleize2 (a:array<'a>) = a.[0], a.[1]

    let inline tupleize3 (a:array<'a>) = a.[0], a.[1], a.[2]

    let inline trim (str:string) = str.Trim()

    let inline pow2 power = 1 <<< power

    let inline swap2 (a,b) = (b,a)

    let kvpKey (kvp:KeyValuePair<'a, _>) = kvp.Key
    let kvpValue (kvp:KeyValuePair<_,'a>) = kvp.Value
