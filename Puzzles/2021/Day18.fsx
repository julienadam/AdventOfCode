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

type Node = {
    mutable Parent: Node option
    mutable Value: NodeValue
    Id: int
}
with 
    member this.SetParent n =
        this.Parent <- Some n
    member this.Display () =
        let rec displayRec (n: Node) =
            match n.Value with
            | Branch (l, r) ->
                printf "["
                displayRec l
                printf ","
                displayRec r
                printf "]"
            | Leaf v ->
                printf "%i" v
        displayRec this
        printfn ""
and NodeValue =
| Branch of Node * Node
| Leaf of int

// printfn "Entering %s" input
let mutable intGen = 0

let getId () =
    let r = intGen;
    intGen <- intGen + 1
    r

let rec readRec (input:string) parent : Node * string=
    match input.[0] with
    | '[' ->
        let left, afterLeft = readRec (input.Substring(1)) None
        assert(afterLeft.[0] = ',')
        let right, afterRight = readRec (afterLeft.Substring(1)) None
        assert(afterRight.[0] = ']')
        let node = { Value = Branch (left, right); Parent = parent; Id = getId () }
        left.SetParent node
        right.SetParent node
        node, afterRight.Substring(1)
    | c when Char.IsDigit(c) ->
        let v = Int32.Parse(c.ToString())
        { Value = Leaf v; Parent = parent; Id = getId ()}, (input.Substring(1))
    | c -> failwithf "Should never encounter a %c" c

let read input = readRec input None |> fst

let left (n:Node) =
    match n.Value with
    | Branch (l, _) -> l
    | _ -> failwithf "Not a branch node"

let right (n:Node) =
    match n.Value with
    | Branch (_, r) -> r
    | _ -> failwithf "Not a branch node"

let leaf (n:Node) =
    match n.Value with
    | Leaf v -> v
    | _ -> failwithf "Not a leaf node"

let branch (n:Node) =
    match n.Value with
    | Branch (left, right) -> left, right
    | _ -> failwithf "Not a branch node"

let addValue (n:Node) v =
    match n.Value with
    | Leaf e -> n.Value <- Leaf (e + v)
    | _ -> failwithf "Not a branch node"


//[[1,2],3]
//[9,[8,7]]
//[[1,9],[8,5]]
//[[[[1,2],[3,4]],[[5,6],[7,8]]],9]
//[[[9,[3,8]],[[0,9],6]],[[[3,7],[4,9]],3]]
//[[[[1,3],[5,3]],[[1,3],[8,7]]],[[[4,9],[6,9]],[[8,2],[7,3]]]]

(read "[1,2]").Display()
(read "[9,[8,7]]").Display()
(read "[[1,9],[8,5]]").Display()
(read "[[[[1,2],[3,4]],[[5,6],[7,8]]],9]").Display()
(read "[[[9,[3,8]],[[0,9],6]],[[[3,7],[4,9]],3]]").Display()
(read "[[[[1,3],[5,3]],[[1,3],[8,7]]],[[[4,9],[6,9]],[[8,2],[7,3]]]]").Display()

// [[[[[9,8],1],2],3],4] -> [[[[0,9],2],3],4]
(*


To explode a pair, the pair's left value is added to the first regular number to the left of the 
exploding pair (if any), and the pair's right value is added to the first regular number to the right 
of the exploding pair (if any). Exploding pairs will always consist of two regular numbers. 

Then, the entire exploding pair is replaced with the regular number 0.
*)
let explode (node:Node) =
    let rec getNumberToTheLeft (n:Node) = 
        match n.Parent with
        | Some n ->
            match n.Value with
            | Branch (_,r) ->
                match r.Value with
                | Leaf leafValue -> Some r
                | _ -> getNumberToTheLeft n
            | _ -> failwithf "Should never have a leaf parent node"
        | None -> None

    let rec getNumberToTheRight (n:Node) = 
        match n.Parent with
        | Some n ->
            match n.Value with
            | Branch (l,_) ->
                match l.Value with
                | Leaf leafValue -> Some l
                | _ -> getNumberToTheRight n
            | _ -> failwithf "Should never have a leaf parent node"
        | None -> None

    // update left and right
    match node.Value with
    | Branch (l,r) ->
        match l.Value, l.Value with
        | Leaf l, Leaf r -> 
            if node.Parent.IsSome then
                let right = getNumberToTheRight node
                if right.IsSome then
                    addValue right.Value l
                let left = getNumberToTheLeft node
                if left.IsSome then
                    addValue left.Value l
            ()
        | _ ->
            node.Display()
            failwithf "Explosion on non regular number"
    | Leaf _ -> 
        failwithf "Explosion on leaf"

    // replace with 0
    match node.Parent with
    | Some p ->
        match branch p with
        | n, r  when n.Id = node.Id ->
            p.Value <- Branch ({ Value = Leaf 0; Parent = node.Parent; Id = getId () }, r)
        | l, n  when n.Id = node.Id ->
            p.Value <- Branch (l, { Value = Leaf 0; Parent = node.Parent; Id = getId () })
        | _ -> failwithf "Neither left nor right was the current node ..."
    | None -> failwithf "Parent is not a branch"


let r = "[[[[[9,8],1],2],3],4]" |> read 
let e = r |> left |> left |> left |> left
r.Display()
explode e
r.Display()
