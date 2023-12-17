namespace AdventOfCode

module AStar =
    type AStarNode<'a> = {
        F: int64
        G: int64
        Data: 'a
    }

module FullAStar = 

    open System.Collections.Generic;

    type Config<'a> = 
        {
            /// <summary>
            /// A method that, given a source, will return its neighbours.
            /// </summary>
            neighbours: 'a -> seq<'a>
            /// <summary>
            /// Given two nodes that are next to each other, return the g cost between them.
            /// The g cost is the cost of moving from one to the other directly.
            /// </summary>
            gCost: 'a -> 'a -> float
            /// <summary>
            /// Given two nodes, return the f cost between them. This is a heuristic score used from a given node to the goal.
            /// Line-of-sight distance is an example of how this might be defined.
            /// </summary>
            fCost: 'a -> 'a -> float
            /// <summary>
            /// The maximum number of tiles to check - used to limit overly long searches when accuracy is not paramount
            /// </summary>
            maxIterations: int option
        }

    let searchWithGoalFunc<'a when 'a : comparison> start goal isGoal config : seq<'a> option =

        let rec reconstructPath cameFrom current =
            seq {
                yield current
                match Map.tryFind current cameFrom with
                | None -> ()
                | Some next -> yield! reconstructPath cameFrom next
            }

        let rec crawler (closedSet:HashSet<'a>) (openSet, gScores : IDictionary<'a, float>, fScores : IDictionary<'a, float>, cameFrom) =
            match config.maxIterations with 
            | Some n when n = closedSet.Count -> None
            | _ ->
                // TODO: optimize sort using a better data structure
                match openSet |> List.sortBy (fun n -> fScores.[n]) with
                | current::_ when isGoal(current) -> Some <| reconstructPath cameFrom current 
                | current::rest ->
                    let gScore = gScores.[current]
                    let next =
                        config.neighbours current 
                        |> Seq.filter (fun n -> closedSet.Contains(n) = false)
                        |> Seq.fold (fun (openSet, gScores : IDictionary<'a, float>, fScores : IDictionary<'a, float>, cameFrom) neighbour ->
                            let tentativeGScore = gScore + config.gCost current neighbour
                            if List.contains neighbour openSet && tentativeGScore >= gScores.[neighbour] then 
                                (openSet, gScores, fScores, cameFrom)
                            else
                                let newOpenSet = if List.contains neighbour openSet then openSet else neighbour::openSet
                                if gScores.ContainsKey neighbour then
                                    gScores.[neighbour] = tentativeGScore |> ignore
                                else
                                    gScores.Add(neighbour, tentativeGScore)

                                let ns =(tentativeGScore + config.fCost neighbour goal)
                                if fScores.ContainsKey neighbour then
                                    fScores.[neighbour] = ns |> ignore
                                else
                                    fScores.Add(neighbour, ns)

                                let newCameFrom = Map.add neighbour current cameFrom
                                newOpenSet, gScores, fScores, newCameFrom
                            ) (rest, gScores, fScores, cameFrom)
                    closedSet.Add(current) |> ignore
                    crawler closedSet next
                | _ -> None

        let gScores = new Dictionary<'a, float>()
        gScores.Add(start, 0.)
        let fScores = new Dictionary<'a, float>()
        fScores.Add(start, config.fCost start goal)
        crawler (new HashSet<'a>()) ([start], gScores, fScores, Map.empty)

    let search start goal config = searchWithGoalFunc start goal (fun c -> c = goal) config