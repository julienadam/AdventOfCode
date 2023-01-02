open System.Diagnostics
open System

#load "../../Tools.fs"

open AdventOfCode
open System.IO

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

(read "[1,2]").Display()
(read "[9,[8,7]]").Display()
(read "[[1,9],[8,5]]").Display()
(read "[[[[1,2],[3,4]],[[5,6],[7,8]]],9]").Display()
(read "[[[9,[3,8]],[[0,9],6]],[[[3,7],[4,9]],3]]").Display()
(read "[[[[1,3],[5,3]],[[1,3],[8,7]]],[[[4,9],[6,9]],[[8,2],[7,3]]]]").Display()

let explode (rootNode: Node) (node:Node) =

    let allLeafNodes = depthFirst rootNode |> Seq.toList
    //allLeafNodes |> Seq.iter (fun n -> match n.Value with | Leaf v -> printfn "%i " v)
    //printfn ""

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

let split (n:Node) =
    let value = n |> leaf
    let vl = Math.Floor ((value |> double) / 2.0) |> int
    let vr = Math.Ceiling ((value |> double) / 2.0) |> int
    let left = { Parent = Some n; Id = getId(); Value = Leaf vl }
    let right = { Parent = Some n; Id = getId(); Value = Leaf vr }
    let newNodeValue = Branch (left, right)
    n.Value <- newNodeValue

let x = "[8,1]" |> read
addValue (x |> left) 1
split (x |> left)
x.Display()

let add n1 n2 =
    let result = { Parent = None; Value = Branch (n1, n2); Id = getId() }
    n1.SetParent result
    n2.SetParent result
    //printfn "%s + %s gives %s" (n1.Display()) (n2.Display()) (result.Display())
    result

let r = add ("[5,4]" |> read) ("[3,2]" |> read)
r.Display()

type ReduceAction =
| Explode of Node
| Split of Node

let getDepth n =
    let mutable d = 1
    let mutable node = n
    while node.Parent.IsSome do
        node <- node.Parent.Value
        d <- d + 1
    d

let findAction n : ReduceAction option =
    let rec findExplodeActionRec (na:Node) depth = 
        // TODO Invariant check on depth and parents
        match na.Value, depth with
        | Branch _ , d when d > 4 ->
            //printfn "Found explosion at depth %i" (getDepth na)
            Explode na |> Some
        | Branch (left, right), _ ->
            (findExplodeActionRec left (depth + 1))  
            |> Option.orElse (findExplodeActionRec right (depth + 1)) 
        | _ -> None
    
    let findSplitAction (na:Node) =
        na 
        |>depthFirst 
        |> Seq.tryFind (fun f -> match f.Value with | Leaf x when x >= 10 -> true | _ -> false) 
        |> Option.map (fun o -> Split o)

    (findExplodeActionRec n 1)
    |>Option.orElse (findSplitAction n)

let rec reduce n =
    match findAction n with
    | Some (Explode e) -> 
        // printfn "Explosion in %s on %s" (n.Display()) (e.Display())
        explode n e
        reduce n
    | Some (Split s) ->
        // printfn "Split in %s on %s" (n.Display()) (s.Display())
        split s
        reduce n
    | None ->
        n

//let test1 = add ("[[[[4,3],4],4],[7,[[8,4],9]]]" |> read) ("[1,1]" |> read)
//let reduced = test1 |> reduce
//assert(reduced.Display() = "[[[[0,7],4],[[7,8],[6,0]]],[8,1]]")

let solve1 inputFile = 
    getInputPath inputFile 
    |> File.ReadAllLines
    |> Seq.map read
    |> Seq.fold (fun sumNode n ->
        match sumNode with
        | None -> 
            Some n
        | Some s ->
            let sum = add s n 
            Some(reduce sum)
        ) None

let assertSample inputFile expectedOutcome =
    let result = solve1 inputFile
    let actual = result.Value.Display()
    if actual <> expectedOutcome then
        failwithf "Expected:\r\n%sActual:\r\n%s" expectedOutcome actual


assertSample "Day18_sample2.txt" "[[[[1,1],[2,2]],[3,3]],[4,4]]"
assertSample "Day18_sample3.txt" "[[[[3,0],[5,3]],[4,4]],[5,5]]"
assertSample "Day18_sample3.txt" "[[[[3,0],[5,3]],[4,4]],[5,5]]"
assertSample "Day18_sample4.txt" "[[[[5,0],[7,4]],[5,5]],[6,6]]"
assertSample "Day18_sample5.txt" "[[[[0,7],4],[[7,8],[6,0]]],[8,1]]"

assertSample "Day18_sample1a.txt" "[[[[4,0],[5,4]],[[7,7],[6,0]]],[[8,[7,7]],[[7,9],[5,0]]]]"
assertSample "Day18_sample1b.txt" "[[[[6,7],[6,7]],[[7,7],[0,7]]],[[[8,7],[7,7]],[[8,8],[8,0]]]]"
assertSample "Day18_sample1c.txt" "[[[[7,0],[7,7]],[[7,7],[7,8]]],[[[7,7],[8,8]],[[7,7],[8,7]]]]"
assertSample "Day18_sample1d.txt" "[[[[7,7],[7,8]],[[9,5],[8,7]]],[[[6,8],[0,8]],[[9,9],[9,0]]]]"
assertSample "Day18_sample1e.txt" "[[[[6,6],[6,6]],[[6,0],[6,7]]],[[[7,7],[8,9]],[8,[8,1]]]]"
assertSample "Day18_sample1f.txt" "[[[[6,6],[7,7]],[[0,7],[7,7]]],[[[5,5],[5,6]],9]]"
assertSample "Day18_sample1g.txt" "[[[[7,8],[6,7]],[[6,8],[0,8]]],[[[7,7],[5,0]],[[5,5],[5,6]]]]"
assertSample "Day18_sample1h.txt" "[[[[7,7],[7,7]],[[8,7],[8,7]]],[[[7,0],[7,7]],9]]"
assertSample "Day18_sample1.txt" "[[[[8,7],[7,7]],[[8,6],[7,7]]],[[[0,7],[6,6]],[8,7]]]"
assertSample "Day18_sample6.txt" "[[[[6,6],[7,6]],[[7,7],[7,0]]],[[[7,7],[7,7]],[[7,8],[9,9]]]]"

let rec mag n =
    match n.Value with
    | Branch (left, right) ->
        3 * (mag left) + 2 * (mag right)
    | Leaf v ->
        v

    (* The magnitude of a pair is 3 times the magnitude of its left element plus 2 times 
        the magnitude of its right element. The magnitude of a regular number is just that number.
    *)

assert("[9,1]" |> read |> mag = 29)
assert("[1,9]" |> read |> mag = 21)
assert("[[9,1],[1,9]]" |> read |> mag = 129)
assert("[[1,2],[[3,4],5]]" |> read |> mag = 143)
assert("[[[[0,7],4],[[7,8],[6,0]]],[8,1]]" |> read |> mag = 1384)
assert("[[[[1,1],[2,2]],[3,3]],[4,4]]" |> read |> mag = 445)
assert("[[[[3,0],[5,3]],[4,4]],[5,5]]" |> read |> mag = 791)
assert("[[[[5,0],[7,4]],[5,5]],[6,6]]" |> read |> mag = 1137)
assert("[[[[8,7],[7,7]],[[8,6],[7,7]]],[[[0,7],[6,6]],[8,7]]]" |> read |> mag = 3488)

// "[[[[6,6],[7,6]],[[7,7],[7,0]]],[[[7,7],[7,7]],[[7,8],[9,9]]]]"
solve1 "Day18_sample6.txt" |> Option.get |> mag |> Dump
solve1 "Day18.txt" |> Option.get |> mag |> Dump

assert(add ("[[2,[[7,7],7]],[[5,8],[[9,3],[0,2]]]]" |> read) ("[[[0,[5,8]],[[1,7],[9,6]]],[[4,[1,2]],[[1,4],2]]]" |> read) |> reduce |> mag = 3993)

let solve2 inputFile =
    let nodes = 
        getInputPath inputFile 
        |> File.ReadAllLines
    
    Seq.allPairs nodes nodes 
    |> Seq.filter (fun (s1,s2) -> s1 <> s2)
    |> Seq.map (fun (s1, s2) -> s1 |> read, s2|> read)
    |> Seq.map (fun (n1, n2) -> (add n1 n2) |> reduce |> mag)
    |> Seq.max
    |> Dump

solve2 "Day18_sample6.txt"

let sw = Stopwatch.StartNew()
let r2 = solve2 "Day18.txt"
printfn "%i. Found in %A" r2 sw.Elapsed
