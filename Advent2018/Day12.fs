namespace Tests

open System
open System.Linq
open NUnit.Framework
open System.Text.RegularExpressions

module Day12 =
    let regex = new Regex("(?<state>[.#]{5})\s=>\s(?<result>[.#])")
    let getGroup (name : string) (expMatch : Match) = expMatch.Groups.[name].ToString()
    let toRule line = regex.Match line |> (fun m -> (getGroup "state" m, getGroup "result" m))
    
    let rules =
        let file = "../../../../../../code/advent/input12.txt"
        let lines = System.IO.File.ReadLines(file)
        lines |> Seq.map toRule |> Map.ofSeq
    let makeMap (initial : string) =
        Seq.zip [0..initial.Length] (initial.ToCharArray())
        |> Seq.map (fun (x,y) -> (x, y.ToString()))
        |> Map.ofSeq
        
    let update (states : Map<int,string>) (rules : Map<string,string>) =
        let step state = 
            if rules.ContainsKey state
            then rules.[state]
            else "."
        let mapVal index =
            if states.ContainsKey index
            then states.[index]
            else "."
        let slice c =
            [c-2;c-1;c;c+1;c+2] |> List.map mapVal |> String.Concat
        let filterState (key, state) =
            match state with 
            | Some x -> Some (key, x)
            | None -> None
        let keys = states |> Seq.map (fun (KeyValue (k,v)) -> k)
        let min = if keys.Count() = 0 then 0 else Seq.min keys
        let max = if keys.Count() = 0 then 0 else Seq.max keys
        let keysToCheck =  [min-4..max+4]
        let newStates = 
            keysToCheck
            |> Seq.map (fun k -> (k, step (slice k)))
            |> Seq.skipWhile (fun (_,v) -> v = ".")
            |> Seq.sortByDescending fst
            |> Seq.skipWhile (fun (_,v) -> v = ".")
        Map.ofSeq newStates
        
    let generations rules iters initial =
        let manyRules = List.replicate iters rules
        List.fold update initial manyRules
        
    let rec generationsCached rules iters initial =
        let rec cachedUpdate ((cache : Set<Map<int,string>>), state) = 
            let newState = update state rules
            if cache.Contains newState
            then cache.Count
            else cachedUpdate ((cache.Add newState), newState)
        let manyRules = Seq.replicate iters rules
        cachedUpdate (Set.empty, initial)
        

module Day12Test =
    [<Test>]
    let Part1Example() =
        let ruleStr = """...## => #
..#.. => #
.#... => #
.#.#. => #
.#.## => #
.##.. => #
.#### => #
#.#.# => #
#.### => #
##.#. => #
##.## => #
###.. => #
###.# => #
####. => #"""
        let rules = ruleStr.Split '\n' |> Array.map Day12.toRule |> Map.ofArray
        let str = "#..#.#..##......###...###"
        let initial = Day12.makeMap str
        let finalStates = 
            Day12.generations rules 20 initial
            |> Map.toList
            |> List.sortBy fst
            |> List.map snd
            |> List.map (fun str -> str.ToCharArray().[0])
        Assert.AreEqual("#....##....#####...#######....#.#..##", finalStates)
        
    [<Test>]
    let Part1() =
        let str = "##....#.#.#...#.#..#.#####.#.#.##.#.#.#######...#.##....#..##....#.#..##.####.#..........#..#...#"
        let initial = Day12.makeMap str
        let sum = 
            Day12.generations Day12.rules 20 initial
            |> Map.toList
            |> List.filter (snd >> (=) "#")
            |> List.map fst
            |> List.sum
        Assert.AreEqual(2349, sum)
        
    [<Test>]
    let Part2CacheTest() =
        let str = "................................................................................................."
        let initial = Day12.makeMap str
        let firstRepeat = 
            Day12.generationsCached Day12.rules 1 initial
        Assert.AreEqual(1, firstRepeat)
        
    // Caching the reults does not work, there is probably some repeating pattern extending out to infinity
    // Another method will have to be used
    //[<Test>]
    //let Part2() =
    //    let str = "##....#.#.#...#.#..#.#####.#.#.##.#.#.#######...#.##....#..##....#.#..##.####.#..........#..#...#"
    //    let initial = Day12.makeMap str
    //    let firstRepeat = 
    //        Day12.generationsCached Day12.rules 1 initial
    //    Assert.AreEqual(2349, firstRepeat)
