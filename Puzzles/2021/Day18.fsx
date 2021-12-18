open System.Diagnostics
open System.Text.RegularExpressions
open System

#load "../../Tools.fsx"

open Tools
open System.IO

//[[1,2],3]
//[9,[8,7]]
//[[1,9],[8,5]]
//[[[[1,2],[3,4]],[[5,6],[7,8]]],9]
//[[[9,[3,8]],[[0,9],6]],[[[3,7],[4,9]],3]]
//[[[[1,3],[5,3]],[[1,3],[8,7]]],[[[4,9],[6,9]],[[8,2],[7,3]]]]


type Node = 
| BranchNode of BranchNode
| LeafNode of SailFishNumber
with 
    member this.SetParent n =
        match this with
        | BranchNode b -> b.Parent <- n
        | LeafNode l -> l.Parent <- n
and BranchNode = {
    mutable Parent: BranchNode option
    Left: Node
    Right: Node
}
and SailFishNumber = {
    mutable Parent : BranchNode option
    Value : int
}

let rec readRec input parent : Node * string=
    printfn "Entering %s" input
    match input.[0] with
    | '[' ->
        let left, afterLeft = readRec (input.Substring(1)) None
        assert(afterLeft.[0] = ',')
        let right, afterRight = readRec (afterLeft.Substring(1)) None
        assert(afterRight.[0] = ']')
        let node = { Left = left; Right = right; Parent = parent }
        node.Left.SetParent (Some node)
        node.Right.SetParent (Some node)
        node |> BranchNode, afterRight.Substring(1)
    | c when Char.IsDigit(c) ->
        LeafNode { Value = Int32.Parse(c.ToString()); Parent = parent}, (input.Substring(1))
    | c -> failwithf "Should never encounter a %c" c

let read input = readRec input None |> fst

//
//[[1,2],3]
//[9,[8,7]]
//[[1,9],[8,5]]
//[[[[1,2],[3,4]],[[5,6],[7,8]]],9]
//[[[9,[3,8]],[[0,9],6]],[[[3,7],[4,9]],3]]
//[[[[1,3],[5,3]],[[1,3],[8,7]]],[[[4,9],[6,9]],[[8,2],[7,3]]]]

read "[1,2]" |> Dump
read "[9,[8,7]]" |> Dump
read "[[1,9],[8,5]]" |> Dump
read "[[[[1,2],[3,4]],[[5,6],[7,8]]],9]" |> Dump
read "[[[9,[3,8]],[[0,9],6]],[[[3,7],[4,9]],3]]" |> Dump
read "[[[[1,3],[5,3]],[[1,3],[8,7]]],[[[4,9],[6,9]],[[8,2],[7,3]]]]" |> Dump

