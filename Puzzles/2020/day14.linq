<Query Kind="FSharpProgram" />

let path = 
    Path.Combine(
        Environment.GetFolderPath(Environment.SpecialFolder.UserProfile), 
        "CloudStation\Data\AdventOfCode\day14.txt")
let input = File.ReadAllLines(path)

let inline groupVal (name:string) (m:Match) = m.Groups.[name].Value

type MaskBit = | On | Off | DoNothing

[<NoComparison>]
type Instruction =
    | Mem of int64 * BitArray
    | Mask of MaskBit array

let parseBit c = match c with | '0' -> Off | '1' -> On | 'X' -> DoNothing | _ -> failwithf "unknown mask bit %c" c

let setBit index i = (1L <<< index) ||| i
let isBitSet index i = ((1L <<< index) &&& i) > 0L

let integerToBitArray (input:int64) =
    [|0..35|] 
    |> Array.map (fun i -> isBitSet i input)
    |> Array.rev
    |> BitArray

let bitArrayToInteger (input:BitArray) =
    input.Cast<bool>()
    |> Seq.fold (fun (x, index) bit ->
        if bit then (setBit index x, index - 1)
        else (x, index - 1)) (0L, input.Length - 1)
    |> fst
    

let bitsToInteger (input:MaskBit array) =
    input
    |> Seq.fold (fun (x, index) bit ->
        match bit with
        | On -> (setBit index x, index - 1)
        | Off -> (x, index - 1)
        | _ -> failwith "Cannot convert X to bitmask")
        (0L, input.Length - 1)
    |> fst
    
    
let parseMask m = m |> Seq.map parseBit |> Seq.toArray
    
let parseLine line =
    let mMem = Regex.Match(line, "mem\[(?<address>\d+)\] = (?<value>\d+)")
    if mMem.Success then
        let address = mMem |> groupVal "address" |> int64
        let value = mMem |> groupVal "value" |> int64 |> integerToBitArray
        Mem (address, value)
    else
        let mMask = Regex.Match(line, "mask = (?<mask>[01X]{36})")
        if mMask.Success then
            Mask (mMask |> groupVal "mask" |> parseMask)
        else
            failwithf "Unrecognized line, neither a mem nor a mask %s" line
            
let applyMask (mask:MaskBit array) (value:BitArray) = 
    mask |> Array.iteri (fun i bit ->
        match bit with
        | On -> value.Set(i, true)
        | Off -> value.Set(i, false)
        | DoNothing -> ())
    value
       
let bitsStr (bits:MaskBit array) =
    bits
    |> Seq.map (fun b -> match b with | On -> '1' | Off -> '0' | DoNothing -> 'X') 
    |> Seq.toArray
    |> System.String
 
module Puzzle1 =

    let solution () =
        input
        |> Seq.map parseLine 
        |> Seq.fold (fun (mask, mem: Map<int64, BitArray>) instruction ->
            match instruction with
            | Mask bits -> (bits, mem)
            | Mem (index, value) ->
                let maskedValue = applyMask mask value
                (mask, mem.Add(index, maskedValue))
            ) (Array.init 36 (fun _ -> DoNothing), Map.empty)
        |> snd
        |> Map.map (fun _ v -> bitArrayToInteger v)
        |> Dump
        |> Seq.sumBy (fun pair -> pair.Value)
        |> Dump

module Puzzle2 =

    let rec getMaskValuesRec index (mask: MaskBit array) (output: MaskBit array list) =
        if index > mask.Length - 1 then
            mask :: output
        else
            match mask.[index] with
            | On ->output @ getMaskValuesRec (index + 1) mask output
            | Off -> output @ getMaskValuesRec (index + 1) mask output
            | DoNothing ->
                let newMaskOn = mask |> Array.copy
                newMaskOn.[index] <- On
                let newMaskOff = mask |> Array.copy
                newMaskOff.[index] <- Off
                getMaskValuesRec (index + 1) newMaskOff output @ getMaskValuesRec (index + 1) newMaskOn output
        
    let getMaskValues (mask: MaskBit array) = getMaskValuesRec 0 mask []
    
    let getAdresses address (mask: MaskBit array) =
        let zipped = Array.zip ((address |> integerToBitArray).Cast<bool>().ToArray()) mask
        let maskedAddress = 
            zipped |> Array.map (fun (addressBit, maskBit) ->
                match addressBit, maskBit with
                | _, On -> On
                | true, Off -> On
                | false, Off -> Off
                | _, DoNothing -> DoNothing)
        
        getMaskValues maskedAddress
            
    let applyMemoryChanges address (mask: MaskBit array) (value:BitArray) (mem: Map<int64, BitArray>) =
        getAdresses address mask
        |> Seq.fold (fun finalMem maskedAddress -> 
            let maskedAddressInt = bitsToInteger maskedAddress
            printfn "Address %i set to %i" maskedAddressInt (value |> bitArrayToInteger)
            finalMem |> Map.add maskedAddressInt value)
            mem
        
    let solution() =
        input
        |> Seq.map parseLine 
        |> Seq.fold (fun (mask, mem: Map<int64, BitArray>) instruction ->
            match instruction with
            | Mask bits -> (bits, mem)
            | Mem (address, value) -> (mask, mem |> applyMemoryChanges address mask value)
            ) (Array.init 36 (fun _ -> DoNothing), Map.empty)
        |> snd
        |> Map.map (fun _ v -> bitArrayToInteger v)
        |> Dump
        |> Seq.sumBy (fun pair -> pair.Value)
        |> Dump

Puzzle2.solution()
