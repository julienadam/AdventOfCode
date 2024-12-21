#time "on"
#load "../../Tools.fs"
#load "../../Tools/SeqEx.fs"
#load "../../Tools/Array2DTools.fs"

open System
open System.IO
open AdventOfCode
open Array2DTools
open Checked

let getInput name = File.ReadAllLines(getInputPath2024 name)

let solve1 input = 
    
    getInput input |> Dump

solve1 "Day21_sample1.txt"

let manhattanPath (ra,ca) (rb,cb) = seq {
    if rb > ra then
        for _ = ra + 1 to rb do yield 'v'
    else if rb < ra then
        for _ = rb + 1 to ra do yield '^'
    if cb > ca then
        for _ = ca + 1 to cb do yield '>'
    else if cb < ca then
        for _ = cb + 1 to ca do yield '<'
}

let allManhattanPaths pA pB =
    let dirs = manhattanPath pA pB |> Seq.toList
    SeqEx.permutations dirs.Length dirs |> Seq.distinct


let numPad = [
    "789"
    "456"
    "321"
    ".0A"
]

let numPadMap = numPad |> array2D |> filteri (fun _ _ v -> v <> '.') |> Seq.map (fun (r,c,v) -> v, (r,c)) |> Map.ofSeq

let allNumPadPaths a b =
    let pA = numPadMap[a] // TODO: Could be constants for a little perf boost
    let pB = numPadMap[b]
    let result = allManhattanPaths pA pB |> Seq.toList
    // Filter out paths that would go through the empty space left of 0
    if pA = (3,1) then // '0'
        result |> List.filter (fun l -> match l with | '<'::_ -> false | _ -> true)
    else if pA = (3,2) then // 'A'
        result |> List.filter (fun l -> match l with | '<'::'<'::_ -> false | _ -> true)
    else if pA = (2,0) then // '3'
        result |> List.filter (fun l -> match l with | 'v'::_ -> false | _ -> true)
    else if pA = (1,0) then // '4'
        result |> List.filter (fun l -> match l with | 'v'::'v'::_ -> false | _ -> true)
    else if pA = (0,0) then // '7'
        result |> List.filter (fun l -> match l with | 'v'::'v'::'v'::_ -> false | _ -> true)
    else
        result

let dirPad = [
    ".^A"
    "<v>"
]

let dirPadMap = dirPad |> array2D |> filteri (fun _ _ v -> v <> '.') |> Seq.map (fun (r,c,v) -> v, (r,c)) |> Map.ofSeq

let allDirPadPaths a b =
    let pA = dirPadMap[a] // TODO: Could be constants for a little perf boost
    let pB = dirPadMap[b]
    let result = allManhattanPaths pA pB |> Seq.toList
    // Filter out paths that would go through the empty space left of 0
    if pA = (0,1) then // '^'
        result |> List.filter (fun l -> match l with | '<'::_ -> false | _ -> true)
    else if pA = (0,2) then // 'A'
        result |> List.filter (fun l -> match l with | '<'::'<'::_ -> false | _ -> true)
    else if pA = (1,0) then // '<'
        result |> List.filter (fun l -> match l with | '^'::_ -> false | _ -> true)
    else
        result

let mapNumPadToDirPad code = 
    ("A" + code) // Insert an extra A to account for the move from A to the first code letter
    |> Seq.pairwise 
    |> Seq.map (fun (a,b) -> 
        let allPathsTob = 
            allNumPadPaths a b 
            |> List.map (fun charList -> sprintf "%s%c" (new String(charList |> List.toArray)) 'A')
        b, allPathsTob)
    |> Seq.toList

let possibleDepressurizedRobotInput = mapNumPadToDirPad "029A" |> Dump

let mapDirPadToDirPad (dirPadCommands:string) =
    ("A" + dirPadCommands) // Insert an extra A to account for the move from A to the first code letter
    |> Seq.pairwise 
    |> Seq.map (fun (a,b) -> 
        let allPathsTob = 
            allDirPadPaths a b 
            |> List.map (fun charList -> sprintf "%s%c" (new String(charList |> List.toArray)) 'A')
        b, allPathsTob)
    |> Seq.toList

let filterBestSequences (c,(dirPadSequences:string list)) =
    let sorted = 
        dirPadSequences
        |> List.map mapDirPadToDirPad
        |> List.map (fun irrSeqs -> irrSeqs, irrSeqs |> List.sumBy (fun s -> (s |> snd |> List.head ).Length))
        |> List.sortBy snd

    // Compute the best path length
    let bestPathLength = sorted.Head |> snd
    printfn "best path for %c %i" c bestPathLength
    c, sorted |> List.takeWhile (fun (_, score) -> score = bestPathLength) |> List.map fst

let getBestDepressurizedRobotInputs (inputs:(char * string list) list) =  inputs |> List.map filterBestSequences

let bestDepressurizedRobotInputs = getBestDepressurizedRobotInputs possibleDepressurizedRobotInput

//let getPossibleFrozenRobotInputs (depressurizedRobotCommands:string) =
//    ("A" + depressurizedRobotCommands) // Insert an extra A to account for the move from A to the first code letter
//    |> Seq.pairwise 
//    |> Seq.map (fun (a,b) -> 
//        let allPathsTob = 
//            allDirPadPaths a b 
//            |> List.map (fun charList -> sprintf "%s%c" (new String(charList |> List.toArray)) 'A')
//        b, allPathsTob)
//    |> Seq.toList

//let getBestFrozenRobotInputs (inputs: (char * (char * string list) list list) list) =
//    ()

[
    ('0', 
        [
            [
                ('<', ["v<<A"; "<v<A"]); 
                ('A', [">^>A"; ">>^A"])
            ]
        ]
    );
    ('2', 
        [
            [
                ('^', ["<A"]); ('A', [">A"])
            ]
        ]
    );
    ('9',
        [
            [
                ('^', ["<A"]); 
                ('^', ["A"]); 
                ('>', ["v>A"; ">vA"]); 
                ('A', ["^A"])
            ]
            [
                ('>', ["vA"]); 
                ('^', ["^<A"; "<^A"]); 
                ('^', ["A"]); 
                ('A', [">A"])
            ]
        ]
    );
    ('A',
        [
            [
                ('v', ["v<A"; "<vA"]); 
                ('v', ["A"]); 
                ('v', ["A"]); 
                ('A', ["^>A"; ">^A"])
            ]
        ]
    )
]

// let getPossibleFrozenRobotInputs 
[
    ('0', 
        [
            [
                ["v<<A"; "<v<A"]; [">^>A"; ">>^A"]
            ]
        ]); 
    ('2', 
        [
            [
                ["<A"]; [">A"]
            ]
        ]);
    ('9', 
        [
            [
                ["<A"]; ["A"]; ["v>A"; ">vA"]; ["^A"]
            ]; 
            [
                ["vA"]; ["^<A"; "<^A"]; ["A"]; [">A"]
            ]
        ]
    );
    ('A', 
        [
            [
                ["v<A"; "<vA"]; ["A"]; ["A"]; ["^>A"; ">^A"]
            ]
        ]
    )
]

//[
//    // 0
//    [ 
//        seq [
//            ["v<<A"; "<v<A"]; [">^>A"; ">>^A"]
//        ]
//    ]; 
//    // 2
//    [
//        seq [
//            ["<A"]; [">A"]
//        ]
//    ];
//    // 9
//    [
//        seq [
//            ["<A"]; ["A"]; ["v>A"; ">vA"]; ["^A"]
//        ];
//        seq [
//            ["<A"]; ["v>A"; ">vA"]; ["^<A"; "<^A"]; [">A"]
//        ];
//        seq [
//            ["vA"]; ["^<A"; "<^A"]; ["A"]; [">A"]
//        ]
//    ];
//    // A
//    [
//        seq [
//            ["v<A"; "<vA"]; ["A"]; ["A"]; ["^>A"; ">^A"]
//        ]
//    ]
//]


// All codes ends on A so we can consider them separately.
// To press the final A, the depressurized robot would need to be on the A, 
// and the irradiated robot on A and the frozen robot on A
// So essentially each code starts with all robots on A


//allDirPadPaths '^' '<'
//allDirPadPaths 'A' '<'
//allDirPadPaths 'v' 'A'
//allDirPadPaths '<' 'A'
//allDirPadPaths '^' '>'

//allNumPadPaths 'A' '7' |> Seq.toArray |> Dump
//allNumPadPaths '0' '7' |> Seq.toArray |> Dump
//allNumPadPaths '3' 'A' |> Seq.toArray |> Dump

//allNumPadPaths '0' '2' |> Seq.toArray |> Dump
//allNumPadPaths '2' '9' |> Seq.toArray |> Dump
//allNumPadPaths '9' 'A' |> Seq.toArray |> Dump


//#r "nuget: quikgraph"
//#r "nuget: quikgraph.GraphViz"

//open QuikGraph
//open QuikGraph.Graphviz

//let g = new QuikGraph.AdjacencyGraph<char, TaggedEdge<char, Direction>>()
//g.AddVertexRange([0..9] |> Seq.map (fun i -> (('0' |> int) + i) |> char));
//g.AddVertex('A')
//g.AddEdge(new TaggedEdge<char, Direction>('0','A',Right))
//g.AddEdge(new TaggedEdge<char, Direction>('A','0',Left))
//g.AddEdge(new TaggedEdge<char, Direction>('0','2',Up))
//g.AddEdge(new TaggedEdge<char, Direction>('2','0',Down))
//g.AddEdge(new TaggedEdge<char, Direction>('A','3', Up))
//g.AddEdge(new TaggedEdge<char, Direction>('3','A', Down))
//g.AddEdge(new TaggedEdge<char, Direction>('1','2', Right))
//g.AddEdge(new TaggedEdge<char, Direction>('2','1', Left))
//g.AddEdge(new TaggedEdge<char, Direction>('1','4', Up))
//g.AddEdge(new TaggedEdge<char, Direction>('4','1', Down))
////g.AddEdge(new Edge<char>('1','4'))
////g.AddEdge(new Edge<char>('2','3'))
////g.AddEdge(new Edge<char>('2','5'))
////g.AddEdge(new Edge<char>('3','6'))
////g.AddEdge(new Edge<char>('4','5'))
////g.AddEdge(new Edge<char>('4','7'))
////g.AddEdge(new Edge<char>('5','6'))
////g.AddEdge(new Edge<char>('5','8'))
////g.AddEdge(new Edge<char>('6','9'))
////g.AddEdge(new Edge<char>('7','8'))
////g.AddEdge(new Edge<char>('7','9'))


//let algo = QuikGraph.Algorithms.ShortestPath.DijkstraShortestPathAlgorithm(g, fun edge -> 1.0)

//let vd = new QuikGraph.Algorithms.Observers.VertexDistanceRecorderObserver<char, TaggedEdge<char, Direction>>(fun edge -> 1.0)
//vd.Attach(algo)

//let vp = new QuikGraph.Algorithms.Observers.VertexPredecessorRecorderObserver<char, TaggedEdge<char, Direction>>()
//vp.Attach(algo)

//algo.Compute('A')
//algo.GetDistances() |> Seq.toArray |> Dump

//for kvp in vd.Distances do
//    Console.WriteLine("Distance from root to node {0} is {1}", kvp.Key, kvp.Value);

//for kvp in vp.VerticesPredecessors do
//    Console.WriteLine("If you want to get to {0} you have to enter through the in edge {1}", kvp.Key, kvp.Value );


//// File.WriteAllText(@"c:\temp\day21\num_keypad.dot", g.ToGraphviz())

//QuikGraph.Algorithms.ShortestPath.

