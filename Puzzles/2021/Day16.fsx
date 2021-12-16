open System.Diagnostics

#load "../../Tools.fsx"

open Tools
open System.IO

let bitsToNumber bits =
    bits 
    |> Seq.rev 
    |> Seq.indexed 
    |> Seq.fold (fun state (i, bit) -> state ||| ((bit |> int64) <<< i)) 0L

let printBits bits prefix =
    printf "%s" prefix
    bits |> Seq.iter (fun i -> printf "%i" i)
    printfn ""

let hexToBits (input:string) =
    input 
    |> Seq.collect (fun c -> 
        match c with
        | '0' -> [0;0;0;0]
        | '1' -> [0;0;0;1]
        | '2' -> [0;0;1;0]
        | '3' -> [0;0;1;1]
        | '4' -> [0;1;0;0]
        | '5' -> [0;1;0;1]
        | '6' -> [0;1;1;0]
        | '7' -> [0;1;1;1]
        | '8' -> [1;0;0;0]
        | '9' -> [1;0;0;1]
        | 'A' -> [1;0;1;0]
        | 'B' -> [1;0;1;1]
        | 'C' -> [1;1;0;0]
        | 'D' -> [1;1;0;1]
        | 'E' -> [1;1;1;0]
        | 'F' -> [1;1;1;1]
        | _ -> failwithf "Invalid hex char %c" c
        )
    |> Seq.toList
    
module Part1 =

    let packetVersions:System.Collections.Generic.List<int> = new System.Collections.Generic.List<int>()
    
    let rec decodePacket input depth =
        let prefix = new System.String('\t', depth)
        printfn "%sDecoding packet from" prefix
        printBits input prefix
    
        let decodeLiteral bits =
            printfn "%sDecoding literal from" prefix
            printBits bits prefix
            let mutable noZeroFound = true
            let mutable read = 0
            let literal = 
                bits 
                |> Seq.chunkBySize 5
                |> Seq.takeWhile (fun b -> 
                    match noZeroFound, b |> Seq.head with
                    | true, 0 ->
                        noZeroFound <- false
                        read <- read + 1
                        true
                    | false, _ -> false
                    | true, 1 -> 
                        read <- read + 1
                        true
                    | _ -> failwithf "Invalid case") 
                |> Seq.collect (fun b -> b |> Seq.skip 1)
                |> bitsToNumber

            printfn "%sDecoded literal %i" prefix literal

            bits |> Seq.skip (5*read)

        let decodeFixedLength bits =
            printfn "%sDecoding fixed length from" prefix
            printBits input prefix
            let fixedLength = bits |> Seq.take 15 |> bitsToNumber |> int
            printfn "%sReading fixed size of %i bits" prefix fixedLength 
            let mutable packetsToDecode = bits |> Seq.skip 15 |> Seq.take fixedLength
            while packetsToDecode |> Seq.isEmpty |> not do
                packetsToDecode <- decodePacket packetsToDecode (depth + 1)
            
            bits |> Seq.skip (15 + fixedLength)

        let decodeNumSubPackets bits =
            printfn "%sDecoding a number of packets from" prefix
            printBits input prefix
            let numSubPackets = bits |> Seq.take 11 |> bitsToNumber |> int
            printfn "%sReading %i sub-packets" prefix numSubPackets
            [1..numSubPackets] 
            |> Seq.fold (fun b _ -> decodePacket b (depth + 1)) (bits |> Seq.skip 11)

        let decodeOperator bits =
            match bits |> Seq.head with
            | 0 -> bits |> Seq.skip 1 |> decodeFixedLength 
            | 1 -> bits |> Seq.skip 1 |> decodeNumSubPackets
            | _ -> failwith "invalid bit"

        let v = input |> Seq.take 3 |> bitsToNumber |> int
        packetVersions.Add(v)
        let typeId = input |> Seq.skip 3 |> Seq.take 3 |> bitsToNumber
        printfn "%sReading packet version %i of type %i" prefix v typeId
        match typeId with
        | 4L -> input |> Seq.skip 6 |> decodeLiteral 
        | op -> input |> Seq.skip 6 |> decodeOperator

    let Solve input = 
        packetVersions.Clear()
        let packetBits = hexToBits input
        decodePacket packetBits 0 |> ignore
        // packetVersions |> Seq.toList |> Dump |> ignore
        (packetVersions |> Seq.sum) |> Dump

module Part2 =
    
    let rec decodePacket input depth =
        let prefix = new System.String('\t', depth)
        printfn "%sDecoding packet from" prefix
        printBits input prefix
    
        let decodeLiteral bits =
            printfn "%sDecoding literal from" prefix
            printBits bits prefix
            let mutable noZeroFound = true
            let mutable read = 0
            let literal = 
                bits 
                |> Seq.chunkBySize 5
                |> Seq.takeWhile (fun b -> 
                    match noZeroFound, b |> Seq.head with
                    | true, 0 ->
                        noZeroFound <- false
                        read <- read + 1
                        true
                    | false, _ -> false
                    | true, 1 -> 
                        read <- read + 1
                        true
                    | _ -> failwithf "Invalid case") 
                |> Seq.collect (fun b -> b |> Seq.skip 1)
                |> bitsToNumber

            printfn "%sDecoded literal %i" prefix literal

            literal, bits |> Seq.skip (5 * read)

        let decodeFixedLength bits =
            printfn "%sDecoding fixed length from" prefix
            printBits input prefix
            let fixedLength = bits |> Seq.take 15 |> bitsToNumber |> int
            printfn "%sReading fixed size of %i bits" prefix fixedLength 
            let values = 
                seq { 
                    let mutable packetsToDecode = bits |> Seq.skip 15 |> Seq.take fixedLength
                    while packetsToDecode |> Seq.isEmpty |> not do
                        let value, remainingBits = decodePacket packetsToDecode (depth + 1)
                        packetsToDecode <- remainingBits
                        yield value
                }
            values |> Seq.toList, bits |> Seq.skip (15 + fixedLength)

        let decodeNumSubPackets bits =
            printfn "%sDecoding a number of packets from" prefix
            printBits input prefix
            let numSubPackets = bits |> Seq.take 11 |> bitsToNumber |> int
            printfn "%sReading %i sub-packets" prefix numSubPackets
            [1..numSubPackets] 
            |> Seq.fold (fun (values, b) _ -> 
                let v, remainingBits = decodePacket b (depth + 1)
                List.append values [v], remainingBits) 
                ([], (bits |> Seq.skip 11))

        let decodeOperator op bits =
            let subValues, remaining = 
                match bits |> Seq.head with
                | 0 -> bits |> Seq.skip 1 |> decodeFixedLength 
                | 1 -> bits |> Seq.skip 1 |> decodeNumSubPackets
                | _ -> failwith "invalid bit"

            let value = 
                match op with
                | 0L -> // Sum
                    printf "%sSum of %A" prefix subValues
                    subValues |> Seq.sum
                | 1L -> // Product
                    printf "%sProduct of %A" prefix subValues
                    subValues |> Seq.fold (fun x v -> x * v) 1L
                | 2L -> // Minimum
                    printf "%sMin of %A" prefix subValues
                    subValues |> Seq.min
                | 3L -> // Maximum
                    printf "%sMax of %A" prefix subValues
                    subValues |> Seq.max
                | 5L -> // Greater than
                    printf "%sGreater than of %A" prefix subValues
                    match subValues with
                    | [a;b] when a > b -> 1L
                    | [_;_] -> 0L
                    | _ -> failwith "Invalid number of subpackets for >"
                | 6L -> // Less than
                    printf "%sLess than than of %A" prefix subValues
                    match subValues with
                    | [a;b] when a < b -> 1L
                    | [_;_] -> 0L
                    | _ -> failwith "Invalid number of subpackets for <"
                | 7L -> // Equal to
                    printf "%sEqual to of%A" prefix subValues
                    match subValues with
                    | [a;b] when a = b -> 1L
                    | [_;_] -> 0L
                    | _ -> failwith "Invalid number of subpackets for ="
                | _ -> failwithf "Operator not implemented %i" op
            
            printfn " = %i" value
            value, remaining


        let v = input |> Seq.take 3 |> bitsToNumber
        let typeId = input |> Seq.skip 3 |> Seq.take 3 |> bitsToNumber
        printfn "%sReading packet version %i of type %i" prefix v typeId
        match typeId with
        | 4L -> input |> Seq.skip 6 |> decodeLiteral 
        | op -> input |> Seq.skip 6 |> decodeOperator op

    let Solve input = 
        let packetBits = hexToBits input
        decodePacket packetBits 0 |> Dump

//assert(bitsToNumber [1;0;0;0] = 8)
//assert(bitsToNumber [1;1;1;1] = 15)
//assert(Part1.Solve "8A004A801A8002F478" = 16)
//assert(Part1.Solve "A0016C880162017C3686B18A3D4780" = 31)
//assert(Part1.Solve "620080001611562C8802118E34" = 12)
//assert(Part1.Solve "C0015000016115A2E0802F182340" = 23)
// getInputPath "Day16.txt" |> File.ReadAllText |> Part1.Solve

assert(Part2.Solve "C200B40A82" |> fst = 3L)
assert(Part2.Solve "04005AC33890" |> fst  = 54L)
assert(Part2.Solve "880086C3E88112" |> fst  = 7L)
assert(Part2.Solve "CE00C43D881120" |> fst  = 9L)
assert(Part2.Solve "D8005AC2A8F0" |> fst = 1L)
assert(Part2.Solve "9C005AC2F8F0" |> fst = 0L)
assert(Part2.Solve "9C0141080250320F1802104A08" |> fst = 1L)
assert(Part2.Solve "F600BC2D8F" |> fst = 0L)

getInputPath "Day16.txt" |> File.ReadAllText |> Part2.Solve