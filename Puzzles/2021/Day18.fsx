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
                sprintf "[%s,%s]" (displayRec l) (displayRec r)
            | Leaf v ->
                sprintf "%i" v
        displayRec this
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

let rec depthFirst (n:Node) = seq {
    match n.Value with 
    | Leaf _ -> yield n
    | Branch (l,r) -> 
        yield! depthFirst l
        yield! depthFirst r
}

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
let explode (rootNode: Node) (node:Node) =
    //let rec getNumberToTheRight (n:Node) = 
    //    match n.Parent with
    //    | Some p ->
    //        match p.Value with
    //        | Branch (_,r) ->
    //            match r.Value with
    //            | Leaf leafValue -> Some r
    //            | _ -> getNumberToTheRight p
    //        | _ -> failwithf "Should never have a leaf parent node"
    //    | None -> None

    //let rec getNumberToTheLeft (n:Node) = 
    //    match n.Parent with
    //    | Some p ->
    //        match p.Value with
    //        | Branch (l,_) ->
    //            match l.Value with
    //            | Leaf leafValue -> Some l
    //            | _ -> getNumberToTheLeft p
    //        | _ -> failwithf "Should never have a leaf parent node"
    //    | None -> None

    let allLeafNodes = depthFirst rootNode |> Seq.toList
    allLeafNodes |> Seq.iter (fun n -> match n.Value with | Leaf v -> printfn "%i " v)
    printfn ""

    // update left and right
    match node.Value with
    | Branch (l,r) ->
        match l.Value, r.Value with
        | Leaf lv, Leaf rv -> 
            let li = allLeafNodes |> List.findIndex (fun f -> f.Id = l.Id)
            if li > 0 then
                addValue allLeafNodes.[li - 1] lv
            let ri = allLeafNodes |> List.findIndex (fun f -> f.Id = r.Id)
            if ri < (allLeafNodes.Length - 1) then
                addValue allLeafNodes.[ri + 1] rv
        | _ -> failwithf "Explosion on non regular number"
    | Leaf _ -> 
        failwithf "Explosion on leaf"
//        if right.IsSome then
    //            addValue right.Value r
    //        let left = getNumberToTheLeft node
    //        if left.IsSome then
    //            addValue left.Value l
    //        ()
    //    | _ ->
    //        printfn "%s" (node.Display())
    //        failwithf "Explosion on non regular number"
    //| Leaf _ -> 
    //    failwithf "Explosion on leaf"

    
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

let assertExplosion (n:Node) (selector:Node->Node) expected =
    let initial = n.Display()
    let toExplode = n |> selector
    explode n toExplode
    let result = n.Display()
    if result <> expected then
        failwithf "%s exploded into %s instead of %s" initial result expected
    else 
        printfn "%s exploded into %s" initial expected 

assertExplosion ("[[[[[9,8],1],2],3],4]" |> read) (left >> left >> left >> left) "[[[[0,9],2],3],4]"
assertExplosion ("[7,[6,[5,[4,[3,2]]]]]" |> read) (right >> right >> right >> right) "[7,[6,[5,[7,0]]]]"
assertExplosion ("[[6,[5,[4,[3,2]]]],1]" |> read) (left >> right >> right >> right) "[[6,[5,[7,0]]],3]"
assertExplosion ("[[3,[2,[8,0]]],[9,[5,[4,[3,2]]]]]" |> read) (right >> right >> right >> right) "[[3,[2,[8,0]]],[9,[5,[7,0]]]]"
assertExplosion ("[[3,[2,[1,[7,3]]]],[6,[5,[4,[3,2]]]]]" |> read) (left >> right >> right >> right) "[[3,[2,[8,0]]],[9,[5,[4,[3,2]]]]]"