
#time
#load "../../Tools.fs"
#r "nuget: faqt"

open System
open System.IO
open AdventOfCode

type HandType = | FiveOf = 10 | FourOf = 9 | FullHouse = 8 | ThreeOf = 7 | TwoPair = 6 | Pair = 5 | Nothing = 4

let inline readCardsInHandGen<^TEnum when ^TEnum: (new: unit -> ^TEnum) and ^TEnum: struct and ^TEnum :> ValueType and ^TEnum: (static member op_Explicit: ^TEnum -> int)> (str:string) =
    str |> Seq.map (fun c -> Enum.Parse<'TEnum>("_" + c.ToString()) |> int) |> Array.ofSeq

[<CustomComparison>]
[<StructuralEquality>]
type Hand = {
    Type : HandType
    Cards : int array
} with
    interface IComparable<Hand> with
        member this.CompareTo(other: Hand) =
            match (this.Type |> int).CompareTo(other.Type |> int) with
            | 0 ->
                let firstDiff = (Seq.zip this.Cards other.Cards) |> Seq.tryFind (fun (a, b) -> a <> b)
                match firstDiff with
                | None -> 0
                | Some (a,b) -> a.CompareTo(b)
            | v -> v
    interface IComparable with
        member this.CompareTo(other) =
            match other with
            | :? Hand as h -> (this :> IComparable<Hand>).CompareTo(h)
            | _ -> failwith("Invalid comparison")

module Part1 = 

    type Part1Card = |_A = 14 |_K = 13 |_Q = 12 |_J = 11 |_T = 10 |_9 = 9  |_8 = 8  |_7 = 7  |_6 = 6 |_5 = 5  |_4 = 4  |_3 = 3  |_2 = 2
    let readCardsInHand (str:string) : int[] = readCardsInHandGen<Part1Card> str

    let classify(cards : int array) =
        if cards.Length <> 5 then failwithf "Should have 5 cards"
        let grouped = 
            cards 
            |> Array.groupBy id 
            |> Array.sortByDescending (fun (g, cards) -> cards.Length)
        match grouped.Length with
        | 1 -> { Type = HandType.FiveOf; Cards = cards}
        | 2 -> 
            match grouped[0] with
            | _, c when c.Length = 4 ->
                { Type = HandType.FourOf; Cards = cards}
            | _, c when c.Length = 3 ->
                { Type = HandType.FullHouse; Cards = cards}
            | _ -> failwithf "2 groups can only be FourOf or FullHouse"
        | 3 -> 
            match grouped[0] with
            | (_,cards1) when cards1.Length = 3 ->
                { Type = HandType.ThreeOf; Cards = cards}
            | (_,cards1) when cards1.Length = 2 ->
                { Type = HandType.TwoPair; Cards = cards}
            | _ -> failwithf "3 groups can only be ThreeOf or TwoPair"
        | 4 -> { Type = HandType.Pair; Cards = cards}
        | 5 -> { Type = HandType.Nothing; Cards = cards}
        | _ -> failwithf "Cannot have more than 5 groups with 5 cards..."

module Part2=

    type Part2Card = |_A = 14 |_K = 13 |_Q = 12 |_T = 10 |_9 = 9  |_8 = 8  |_7 = 7  |_6 = 6 |_5 = 5  |_4 = 4  |_3 = 3  |_2 = 2  |_J = 1 

    let readCardsInHand (str:string) : int[] = readCardsInHandGen<Part2Card> str
    let classify(cards : int array) =
            let J = Part2Card._J |> int
            if cards.Length <> 5 then failwithf "Should have 5 cards"
            let numJokers = cards |> Seq.filter(fun c -> c = J) |> Seq.length
            
            let grouped = 
                match numJokers with
                | 0 -> 
                    // No joker, proceed as usual
                    cards 
                    |> Array.groupBy id
                    |> Array.sortByDescending (fun (g, c) -> (c.Length, g))
                | 5 -> 
                    // Special case for all jokers, there is no other group
                    [|J, [|J;J;J;J;J|]|]
                | _ ->
                    // Find jokers and remove them
                    // Add as many duplicates of the card present in the biggest and strongest group
                    let sorted = 
                        cards 
                        |> Array.filter (fun c -> c <> J)
                        |> Array.groupBy id
                        |> Array.sortByDescending (fun (g, c) -> (c.Length, g))
                    let bestGroupCard, bestCards = sorted.[0]
                    let replacements = [|1..bestCards.Length + numJokers|] |> Array.map (fun _ -> bestCards[0])
                    Array.set sorted 0 (bestGroupCard, replacements)
                    sorted

            match grouped.Length with
            | 1 -> { Type = HandType.FiveOf; Cards = cards}
            | 2 -> 
                match grouped[0] with
                | _, c when c.Length = 4 ->
                    { Type = HandType.FourOf; Cards = cards}
                | _, c when c.Length = 3 ->
                    { Type = HandType.FullHouse; Cards = cards}
                | _ -> failwithf "2 groups can only be FourOf or FullHouse"
            | 3 -> 
                match grouped[0] with
                | (_,cards1) when cards1.Length = 3 ->
                    { Type = HandType.ThreeOf; Cards = cards}
                | (_,cards1) when cards1.Length = 2 ->
                    { Type = HandType.TwoPair; Cards = cards}
                | _ -> failwithf "3 groups can only be ThreeOf or TwoPair"
            | 4 -> { Type = HandType.Pair; Cards = cards}
            | 5 -> { Type = HandType.Nothing; Cards = cards}
            | _ -> failwithf "Cannot have more than 5 groups with 5 cards..."


let solve reader classifier input =
    let getInput name = 
        File.ReadAllLines(getInputPath2023 name)
        |> Array.map (ssplit2 " ")
        |> Array.map (fun (a,b) -> a |> reader, b |> int)

    getInput input 
    |> Seq.map (fun (cards, bid) -> classifier(cards), bid)
    |> Seq.sortBy(fst)
    |> Seq.mapi (fun i (h,bid) -> bid * (i+1))
    |> Seq.sum

let solve1 = solve Part1.readCardsInHand Part1.classify
let solve2 = solve Part2.readCardsInHand Part2.classify


solve1 "Day07_sample.txt"
solve1 "Day07.txt"
printfn "Part 2"
solve2 "Day07_sample.txt"
solve2 "Day07.txt"


module Part1Tests =
    
    open Part1
    open Faqt

    (fun () -> readCardsInHand "AAAA" |> classify).Should().Throw<Exception, _>()
    (fun () -> readCardsInHand "AAAAAA" |> classify).Should().Throw<Exception, _>()
    (readCardsInHand "AAAAA" |> classify).Type.Should().Be(HandType.FiveOf)
    (readCardsInHand "AAAA2" |> classify).Type.Should().Be(HandType.FourOf)
    (readCardsInHand "AA22A" |> classify).Type.Should().Be(HandType.FullHouse)
    (readCardsInHand "AA32A" |> classify).Type.Should().Be(HandType.ThreeOf)
    (readCardsInHand "A332A" |> classify).Type.Should().Be(HandType.TwoPair)
    (readCardsInHand "A342A" |> classify).Type.Should().Be(HandType.Pair)
    (readCardsInHand "AKQT4" |> classify).Type.Should().Be(HandType.Nothing)

    (readCardsInHand "33333" |> classify).Should().BeGreaterThan(readCardsInHand "22222" |> classify)
    (readCardsInHand "33333" |> classify).Should().BeGreaterThan(readCardsInHand "AAAA2" |> classify)
    (readCardsInHand "88883" |> classify).Should().BeGreaterThan(readCardsInHand "88882" |> classify)
    (readCardsInHand "88883" |> classify).Should().BeGreaterThan(readCardsInHand "99922" |> classify)
    (readCardsInHand "44433" |> classify).Should().BeGreaterThan(readCardsInHand "44422" |> classify)
    (readCardsInHand "55533" |> classify).Should().BeGreaterThan(readCardsInHand "66643" |> classify)
    (readCardsInHand "44453" |> classify).Should().BeGreaterThan(readCardsInHand "44452" |> classify)
    (readCardsInHand "44453" |> classify).Should().BeGreaterThan(readCardsInHand "44332" |> classify)
    (readCardsInHand "44332" |> classify).Should().BeGreaterThan(readCardsInHand "3322A" |> classify)
    (readCardsInHand "44332" |> classify).Should().BeGreaterThan(readCardsInHand "AA432" |> classify)
    (readCardsInHand "22KQT" |> classify).Should().BeGreaterThan(readCardsInHand "KQTJ9" |> classify)
