<Query Kind="FSharpProgram" />

let path = Path.Combine(Environment.GetFolderPath(Environment.SpecialFolder.UserProfile), "CloudStation\Data\AdventOfCode\day5.txt")
let input = File.ReadAllLines(path)

let findRow sevenCharCode =
    let f (l, h) code =
        match code with 
        | 'F' -> (l, l + (h - l) / 2) 
        | 'B' -> (l + (h - l) / 2 + 1, h)
        | _ -> failwithf "Unknown code %c" code
    sevenCharCode |> Seq.fold f (0, 127) |> fst

let findColumn threeCharCode =
    let f (l, h) code =
        match code with 
        | 'L' -> (l, l + (h - l) / 2) 
        | 'R' -> (l + (h - l) / 2 + 1, h)
        | _ -> failwithf "Unknown code %c" code
    threeCharCode |> Seq.fold f (0, 7) |> fst

let inline seatId row column = row * 8 + column

let splitCode input = 
    input |> Seq.take 7, input |> Seq.skip 7 |> Seq.take 3

let parseTicket input =
    let cRow, cCol = input |> splitCode
    let c, r = (cRow |> findRow, cCol |> findColumn)
    seatId c r, c, r


let s1 () =

    parseTicket "BFFFBBFRRR" |> Dump // row 70, column 7, seat ID 567.
    parseTicket "FFFBBBFRRR" |> Dump // row 14, column 7, seat ID 119.
    parseTicket "BBFFBBFRLL" |> Dump // row 102, column 4, seat ID 820.
    input |> Seq.map parseTicket |> Seq.map (fun (s, _, _) -> s) |> Seq.max |> Dump

let s2 () =

    input |> Seq.map parseTicket |> Seq.map (fun (s, _, _) -> s) |> Seq.sort |> Seq.windowed 3 |> Seq.where (fun t -> t.[1] = t.[0] + 2) |> Dump
    

s2 ()