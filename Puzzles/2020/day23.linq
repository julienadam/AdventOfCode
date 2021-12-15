<Query Kind="FSharpProgram" />

// let input = [3;8;9;1;2;5;4;6;7]
let input = [9;2;5;1;7;6;8;3;4]

module Puzzle1 =
    let getPossibleDestinations toFind =
        if toFind < 1 || toFind > 9 then
            failwithf "%i is not a valid cup" toFind
        [1..9] |> List.map (fun i -> 
            if toFind - i <= 0 then toFind - i + 9 else toFind - i)
    
    // Naïve implementation, easy to write but very, very slow
    let crabMove (input:int list) =
        let removed = input.[1..3]
        let remaining = input.[0..0] @ input.[4..]
        let possibleDestinations = getPossibleDestinations input.[0]
        let destination = 
            possibleDestinations 
            |> Seq.map (fun d -> remaining |> Seq.tryFindIndex (fun i -> i = d)) 
            |> Seq.pick id
        
        let final = remaining.[0..destination] @ removed @ remaining.[destination + 1..]
        final.Tail @ [final.Head]
        
    let printResult input =
        let index1 = input |> List.findIndex (fun i -> i = 1)
        let ordered = input.[index1 + 1..] @ input.[0..index1 - 1]
        ordered |> Seq.iter (fun i -> printf "%i" i)
    
    let solution() =
        [1..100] |> Seq.fold (fun i _ -> crabMove i) input |> Dump |> printResult

/// ---------------------------- Fast solution ----------------------------------

/// Treats the input linked list as a ring and enumerates exactly Count
/// elements, starting at the specified node
let enumRing (input:LinkedList<'a>) (current:LinkedListNode<'a>) = seq {
    let mutable c = current
    for _ = 0 to input.Count - 1 do
        yield c
        c <- if c.Next = null then input.First else c.Next
}
    
let rec getDestinationCupLabelRec max value (removed: int list) =
    // Wraps to max
    let v = if value - 1 < 1 then max else value - 1
    if removed |> List.contains v |> not then
        v // Value is not in the removed list
    else
        // Value is in the removed list, try the next one
        getDestinationCupLabelRec max v removed
    
// Maintain a Dictionary for finding the cups by label in O(1)
// And a LinkedList for performing the moves in O(1)
let crabMove (inputSet:Dictionary<int, LinkedListNode<int>>) (input:LinkedList<int>) (current:LinkedListNode<int>) max =
    
    // Remove the 3 cups after the current one from the dictionary and the linked list
    let toRemove = enumRing input current |> Seq.skip 1 |> Seq.take 3 |> Seq.toList
    toRemove |> Seq.iter (fun i -> 
        input.Remove(i)
        inputSet.Remove(i.Value) |> ignore)
    let removed = toRemove |> Seq.map (fun n -> n.Value) |> Seq.toList
    
    // Find the destination label and the corresponding cup, using the dictionary
    let destinationCupLabel = getDestinationCupLabelRec max current.Value removed 
    let destinationCup = inputSet.[destinationCupLabel]
    
    // Add the removed cups back in the dictionary and in the linked list after the destination cup
    let added1 = input.AddAfter(destinationCup, removed.[0])
    inputSet.Add(removed.[0], added1)
    let added2 = input.AddAfter(added1, removed.[1])
    inputSet.Add(removed.[1], added2)
    let added3 = input.AddAfter(added2, removed.[2])
    inputSet.Add(removed.[2], added3)
    
    // Return the list and move the position to the next item
    input, ((enumRing input current) |> Seq.skip 1 |> Seq.head)

module Puzzle1b =
    let solution() =
        let fullInput = input |> LinkedList
        let fullInputSet = (enumRing fullInput fullInput.First).ToDictionary(fun n -> n.Value)
        
        let max = 9
        let (afterMoves, _) = 
            [1..100] 
            |> Seq.fold (fun (i, c) _ -> crabMove fullInputSet i c max) (fullInput,fullInput.First) 
        
        let one = enumRing afterMoves afterMoves.First |> Seq.find (fun i -> i.Value = 1)
        enumRing afterMoves one |> Seq.skip 1 |> Seq.take 8 |> Seq.map (fun i -> i.Value)
       
module Puzzle2 =
    
    let getFullInput input = 
        input @ [10..1000000]
    
    let solution() =
        let fullInput = input |> getFullInput |> LinkedList
        let fullInputSet = (enumRing fullInput fullInput.First).ToDictionary(fun n -> n.Value)
        
        let max = fullInput |> Seq.max
        printfn "%s Starting" (DateTime.Now.ToString("HH:mm:ss:fff"))
        let sw = Stopwatch.StartNew()
        let (afterMoves, _) = 
            [1..10000000] 
            |> Seq.fold (fun (i, c) round -> 
                if sw.ElapsedMilliseconds > 10000L then
                    printfn "%s Round %i" (DateTime.Now.ToString("HH:mm:ss:fff")) round
                    sw.Restart()
                
                crabMove fullInputSet i c max
                ) (fullInput, fullInput.First) 
        
        printfn "%s Finished all iterations" (DateTime.Now.ToString("HH:mm:ss:fff"))
        
        let one = enumRing afterMoves afterMoves.First |> Seq.find (fun i -> i.Value = 1)
        enumRing afterMoves one 
        |> Seq.skip 1 
        |> Seq.take 2 
        |> Seq.map (fun i -> i.Value |> int64)
        |> Seq.fold (*) 1L
        
// (934001L * 159792L    ) |> Dump   
Puzzle2.solution() |> Dump