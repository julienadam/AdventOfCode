namespace AdventOfCode

[<AutoOpen>]
module RegexTools =
    open System.Text.RegularExpressions
    let inline mInt (groupName:string) (m:Match) = m.Groups.[groupName].Value |> int
    let inline mInt64 (groupName:string) (m:Match) = m.Groups.[groupName].Value |> int64
    let inline mStr (groupName:string) (m:Match) = m.Groups.[groupName].Value
