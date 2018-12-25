namespace Tests

open System
open NUnit.Framework
open System.Text.RegularExpressions

[<TestClass>]
type Day7 () =
    [<Test>]
    member this.Part1() =
        let regex = new Regex("Step (?<step>.) must be finished before step (?<requires>.) can begin.")
        let file = "../../../../../../code/advent/input7.txt"
        let lines = System.IO.File.ReadLines(file)
        let getGroup (name : string) (expMatch : Match) = expMatch.Groups.[name].ToString()
        let mapMaker map (step, requires) =
            let curVal = 
                match Map.tryFind requires map with 
                | Some x -> x
                | None -> Set.empty
            Map.add requires (Set.add step curVal) map
        let data =
            lines 
            |> Seq.map regex.Match
            |> Seq.map (fun m -> (getGroup "step" m, getGroup "requires" m))
        let addSteps set (first, second) = set |> Set.add first |> Set.add second
        let steps = Seq.fold addSteps Set.empty data
        let reqMap = Set.toList steps |> Seq.map (fun x -> (x, Set.empty)) |> Map.ofSeq
        let requirements = Seq.fold mapMaker reqMap data
        let firstReq reqs =
            let available = Map.filter (fun _ v -> v = Set.empty) reqs |> Map.toSeq |> Seq.map fst |> Seq.sort
            Seq.tryHead available
        let removeStep step key value =
            Set.remove step value
        let rec doStep (reqs : Map<string, Set<string>>) =
            match firstReq reqs with 
            | Some step ->
                let newReqs = reqs |> Map.map (removeStep step) |> Map.remove step
                step :: doStep newReqs
            | None -> []
        let res = doStep requirements
        raise (new Exception(String.Concat res)) |> ignore
        
    [<Test>]
    member this.Part2() =
        let regex = new Regex("Step (?<step>.) must be finished before step (?<requires>.) can begin.")
        let file = "../../../../../../code/advent/input7.txt"
        let lines = System.IO.File.ReadLines(file)
        let getGroup (name : string) (expMatch : Match) = expMatch.Groups.[name].ToString().ToCharArray().[0]
        let mapMaker map (step, requires) =
            let curVal = 
                match Map.tryFind requires map with 
                | Some x -> x
                | None -> Set.empty
            Map.add requires (Set.add step curVal) map
        let data =
            lines 
            |> Seq.map regex.Match
            |> Seq.map (fun m -> (getGroup "step" m, getGroup "requires" m))
        let addSteps set (first, second) = set |> Set.add first |> Set.add second
        let steps = Seq.fold addSteps Set.empty data
        let reqMap = Set.toList steps |> Seq.map (fun x -> (x, Set.empty)) |> Map.ofSeq
        let requirements = Seq.fold mapMaker reqMap data
        let possible working reqs =
            let available = Map.filter (fun _ v -> v = Set.empty) reqs |> Map.toSeq |> Seq.map fst |> Seq.sort
            Seq.except working available
        let removeStep step key value =
            Set.remove step value
        let rec doStep (time : int) (inProgress : seq<(char * int)>) (reqs : Map<char, Set<char>>) =
            let complete = Seq.filter (fun (_, completeAt) -> completeAt <= time) inProgress |> Seq.sort
            let stillInProgress = Seq.except complete inProgress
            let removeStep req step = req |> Map.map (removeStep step) |> Map.remove step
            let newReqs = Seq.fold removeStep reqs (Seq.map fst complete)
            let steps = newReqs |> possible (Seq.map fst stillInProgress) |> Seq.toList
            let takeable = min (5-Seq.length stillInProgress) steps.Length
            let starting = Seq.take takeable steps |> Seq.map (fun step -> (step, 60 + time + int step - (int 'A' - 1)))
            let newProgress = Seq.concat [stillInProgress; starting]
            if Map.isEmpty reqs
            then 
                complete
            else
                Seq.concat [complete; doStep (newProgress |> Seq.map snd |> Seq.fold min Int32.MaxValue) (newProgress |> Seq.toList) newReqs]
        let res = doStep 0 [] requirements |> Seq.maxBy snd |> snd
        raise (new Exception(res.ToString())) |> ignore
