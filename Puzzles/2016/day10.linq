<Query Kind="FSharpProgram" />

let toString : char seq -> string = Seq.map string >> String.concat ""
let getInputPath file = Path.Combine(Environment.GetFolderPath(Environment.SpecialFolder.UserProfile), @"CloudStation\Data\AdventOfCode\2016", file)
let path = getInputPath "day10.txt"
let inline mInt (name:string) (m:Match) = m.Groups.[name].Value |> int

type BotId = int

type DestinationType =
| Bot of BotId
| Output of int

type GiveTargets = {
    LowRule : DestinationType
    HighRule : DestinationType
}

type Instructions =
| Value of BotId*int
| Give of BotId*GiveTargets

let parseValue line = 
    let m = Regex.Match(line, "value (\d+) goes to bot (\d+)")
    Value (m |> mInt "2", m |> mInt "1")

let getDestinationType t v =
    match t with 
    | "output" -> Output (v |> int)
    | "bot" -> Bot (v |> int)
    | _ -> failwithf "Not a valid destination type %s" t

let parseBot line = 
    let m = Regex.Match(line, "bot (\d+) gives low to (output|bot) (\d+) and high to (output|bot) (\d+)")
    let lowRule = getDestinationType (m.Groups.["2"].Value) (m.Groups.["3"].Value)
    let highRule = getDestinationType (m.Groups.["4"].Value) (m.Groups.["5"].Value)
    Give (m |> mInt "1", { LowRule = lowRule; HighRule = highRule })

let parseLine (line:string) =
    if line.StartsWith("value") then
        parseValue line
    else if line.StartsWith("bot") then
        parseBot line
    else
        failwithf "Not a valid line %s" line
        
type Bot = {
    Id: BotId
    Low: int option
    High: int option
    TargetRules: GiveTargets option
}
with
    member this.GetValues() = seq {
        if this.Low.IsSome then yield this.Low.Value
        if this.High.IsSome then yield this.High.Value
    }
    member this.AddValue(v) =
        // Uncomment to get solution 1
        //let vs = this.GetValues()
        //if (v = 61 && vs |> Seq.contains 17) || (v = 17 && vs |> Seq.contains 61) then
        //    failwithf "Bot %i just compared 17 and 61" this.Id
            
        match this.Low, this.High with
        | None, None -> { this with Low = Some v }
        | Some l, None when v >= l -> { this with High = Some v }
        | Some l, None when v < l  -> { this with Low = Some v; High = Some l }
        | None, Some h when v <= h  -> { this with Low = Some v }
        | None, Some h when v > h -> { this with Low = Some h; High = Some v }
        | Some _, Some _ -> failwith "Should not be able to give to a full bot"
    member this.ShouldGive() =
        match this.Low, this.High with
        | Some _, Some _ -> true
        | _ -> false
        
let instructions = 
    File.ReadAllLines(path) 
    |> Seq.map parseLine
    
let bots = Dictionary<int, Bot>()
let getBot botId =
    match bots.TryGetValue(botId) with
    | true, b -> b
    | false, _ -> 
        let newBot = { Id = botId; Low = None; High = None; TargetRules = None }
        bots.Add(botId, newBot)
        newBot
        
// Load the instructions and build the Bot objects
instructions 
|> Seq.iter (fun i -> 
    match i with 
    | Value (id, v) -> 
        let bot = getBot id
        bots.[id] <- bot.AddValue(v)
    | Give (id, targets) ->
        let bot = getBot id
        bots.[id] <- { bot with TargetRules = Some targets }
)

let outputs = Dictionary<int, int>()

// Went with a mutable solution, thinking the second phase would be more
// perf hungry ... turns out it wasn't so I could have used mutable maps
let rec processUntilAllDelivered () =
    let botsToActivate = bots |> Seq.where (fun kvp -> kvp.Value.ShouldGive()) |> Seq.toList
    
    if botsToActivate |> List.isEmpty then
        ()
    else
        botsToActivate |> Seq.iter (fun kvp ->
            let bot = kvp.Value
            match bot.TargetRules.Value.LowRule with
                | Output outputId -> 
                    outputs.Add(outputId, bot.Low.Value)
                    bots.[kvp.Key] <- { bot with Low = None }
                | Bot otherBotId -> 
                    let otherBot = (getBot otherBotId).AddValue(bot.Low.Value)
                    bots.[otherBotId] <- otherBot
                    bots.[kvp.Key] <- { bot with Low = None }
            
            match bot.TargetRules.Value.HighRule with
                | Output outputId -> 
                    outputs.Add(outputId, bot.High.Value)
                    bots.[kvp.Key] <- { bot with High = None }
                | Bot otherBotId -> 
                    let otherBot = (getBot otherBotId).AddValue(bot.High.Value)
                    bots.[otherBotId] <- otherBot
                    bots.[kvp.Key] <- { bot with High = None })
            
        processUntilAllDelivered ()

processUntilAllDelivered () 

outputs.[0] * outputs.[1] * outputs.[2] |> Dump
