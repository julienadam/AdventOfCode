namespace AdventOfCode

open System.Collections.Generic

module AStar =
    type AStarNode<'a> = {
        F: int64
        G: int64
        Data: 'a
    }

module FullAStar = 
    type Config<'a> = 
        {
            /// <summary>
            /// A method that, given a source, will return its neighbors.
            /// </summary>
            neighbors: 'a -> seq<'a>
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

        let closedSet = new HashSet<'a>()

        // Open set is stored in two forms
        // A priority queue for next best selection
        let openSet = new PriorityQueue<'a, float>()
        openSet.Enqueue(start, 0)

        // And a hash set for inclusion testing
        let opens = new HashSet<'a>()
        opens.Add(start) |> ignore

        /// Dequeues from the priority queue and removes from set
        let dequeue () =
            match openSet.TryDequeue() with
            | false, _, _ -> None
            | true, x, _ -> 
                opens.Remove(x) |> ignore
                Some x

        /// Enqueues on priority queue and adds to set
        let enqueue x score =
            opens.Add(x) |> ignore
            openSet.Enqueue(x, score)

        let gScores = new Dictionary<'a, float>()
        gScores.Add(start, 0.)

        let fScores = new Dictionary<'a, float>()
        fScores.Add(start, config.fCost start goal)

        let update key value (dict:IDictionary<'k,'v>) =
            if dict.ContainsKey key then
                dict.[key] <- value
            else
                dict.Add(key, value)

        let rec crawler cameFrom =
            match config.maxIterations with 
            | Some n when n = closedSet.Count -> 
                None
            | _ ->
                match dequeue () with
                | None -> 
                    None
                | Some current when isGoal(current) -> 
                    Some <| reconstructPath cameFrom current 
                | Some current -> 
                    let gScore = gScores.[current]
                    let next =
                        config.neighbors current 
                        |> Seq.filter (fun n -> closedSet.Contains(n) = false)
                        |> Seq.fold (fun cameFrom neighbor ->
                            let tentativeGScore = gScore + config.gCost current neighbor
                            let neighborIsOpen = opens.Contains(neighbor)
                            if neighborIsOpen && tentativeGScore >= gScores.[neighbor] then 
                                cameFrom
                            else
                                let ns = tentativeGScore + (config.fCost neighbor goal)

                                if neighborIsOpen = false then 
                                    enqueue neighbor ns

                                gScores |> update neighbor tentativeGScore
                                fScores |> update neighbor ns

                                Map.add neighbor current cameFrom
                            ) cameFrom
                    closedSet.Add(current) |> ignore
                    crawler next

        crawler Map.empty

    let search start goal config = searchWithGoalFunc start goal (fun c -> c = goal) config