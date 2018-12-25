namespace Tests

open System
open System.Linq
open NUnit.Framework
open System.Text.RegularExpressions
open System.Threading

type Info = 
    | Wakes
    | Falls
    | Begins of int
        
[<TestClass>]
type Day4Class () =

    [<SetUp>]
    member this.Setup () =
        ()

    [<Test>]
    member this.Day4Part1 () =
        let timeRegex = new Regex("\[(?<time>.*)\] (?<info>.*)")
        let file = "../../../../../../code/advent/input4.txt"
        let lines = System.IO.File.ReadLines(file)
        let toTime str = DateTime.Parse str
        let toInfo str = 
            let getGuardId str =
                let timeRegex = new Regex("Guard #(?<id>\\d+) begins shift")
                int (timeRegex.Match(str).Groups.["id"].ToString())
            match str with 
            | "wakes up" -> Wakes
            | "falls asleep" -> Falls
            | strBegins -> Begins (getGuardId strBegins)
        let getGroup (name : string) (expMatch : Match) = expMatch.Groups.[name].ToString()
        let addToMap map (id, (dt1 :DateTime), dt2) =
            let minutes = (dt2 - dt1).TotalMinutes
            if Map.containsKey id map
            then 
                map.Add (id, map.[id] + minutes)
            else
                map.Add (id, minutes)
        let folder (list, curId, fell) (dt, info) = 
            match info with 
            | Begins id -> (list, id, dt)
            | Falls -> (list, curId, dt)
            | Wakes -> ((curId,fell,dt) :: list, curId, fell)
        let data =
            lines
            |> Seq.map timeRegex.Match
            |> Seq.map (fun m -> (getGroup "time" m, getGroup "info" m))
            |> Seq.map (fun (time, info) -> (DateTime.Parse time, toInfo info))
            |> Seq.sortBy (fun (time, info) -> time)
        let (sleeps, _, _) = data |> Seq.fold folder ([], 0, DateTime.Parse "1518-09-14 00:01")
        let map = Seq.fold addToMap Map.empty sleeps
        let (sleepiestId, _) = List.maxBy snd (Map.toList map)
        let sleepsOfSleepiest = sleeps |> Seq.filter (fun (id, _, _) -> id = sleepiestId)
        let incrementMinuteRange (map : Map<int, int>) (_, (dt1 : DateTime),(dt2 : DateTime)) : Map<int, int> =
            let range = Enumerable.Range(dt1.Minute, dt2.Minute - dt1.Minute)
            let incrementMinute map minute = 
                if Map.containsKey minute map
                then 
                    map.Add (minute, map.[minute] + 1)
                else
                    map.Add (minute, 1)
            range |> Seq.fold (incrementMinute) map
        let sleepsPerMinute = sleepsOfSleepiest |> Seq.fold incrementMinuteRange Map.empty |> Map.toSeq
        let kvp = Seq.maxBy snd sleepsPerMinute
        let res = fst kvp * sleepiestId
        raise (new Exception(res.ToString())) |> ignore

    [<Test>]
    member this.Day4Part2 () =
        let timeRegex = new Regex("\[(?<time>.*)\] (?<info>.*)")
        let file = "../../../../../../code/advent/input4.txt"
        let lines = System.IO.File.ReadLines(file)
        let toTime str = DateTime.Parse str
        let toInfo str = 
            let getGuardId str =
                let timeRegex = new Regex("Guard #(?<id>\\d+) begins shift")
                int (timeRegex.Match(str).Groups.["id"].ToString())
            match str with 
            | "wakes up" -> Wakes
            | "falls asleep" -> Falls
            | strBegins -> Begins (getGuardId strBegins)
        let getGroup (name : string) (expMatch : Match) = expMatch.Groups.[name].ToString()
        let addToMap map (id, (dt1 :DateTime), dt2) =
            let minutes = (dt2 - dt1).TotalMinutes
            if Map.containsKey id map
            then 
                map.Add (id, map.[id] + minutes)
            else
                map.Add (id, minutes)
        let folder (list, curId, fell) (dt, info) = 
            match info with 
            | Begins id -> (list, id, dt)
            | Falls -> (list, curId, dt)
            | Wakes -> ((curId,fell,dt) :: list, curId, fell)
        let data =
            lines
            |> Seq.map timeRegex.Match
            |> Seq.map (fun m -> (getGroup "time" m, getGroup "info" m))
            |> Seq.map (fun (time, info) -> (DateTime.Parse time, toInfo info))
            |> Seq.sortBy (fun (time, info) -> time)
        let (sleeps, _, _) = data |> Seq.fold folder ([], 0, DateTime.Parse "1518-09-14 00:01")
        let map = Seq.fold addToMap Map.empty sleeps
        let incrementMinuteRange (map : Map<int, int>) (_, (dt1 : DateTime),(dt2 : DateTime)) : Map<int, int> =
            let range = Enumerable.Range(dt1.Minute, dt2.Minute - dt1.Minute)
            let incrementMinute map minute = 
                if Map.containsKey minute map
                then 
                    map.Add (minute, map.[minute] + 1)
                else
                    map.Add (minute, 1)
            range |> Seq.fold (incrementMinute) map
        let sleepsPerMinute id = Seq.filter (fun (i,_,_) -> i = id) sleeps |> Seq.fold incrementMinuteRange Map.empty |> Map.toList |> Seq.maxBy snd
        let sleepsPerMinuteFst id = Seq.filter (fun (i,_,_) -> i = id) sleeps |> Seq.fold incrementMinuteRange Map.empty |> Map.toList |> Seq.maxBy snd
        let guards = sleeps |> Seq.map (fun (id,_,_) -> id)  |> Seq.distinct //|> Seq.fold (fun map id -> Map.add id Map.empty map) Map.empty
        let guard = guards |> Seq.maxBy (fun guard -> snd (sleepsPerMinute guard))
        let minute = fst (sleepsPerMinute guard)
        raise (new Exception((guard * minute).ToString())) |> ignore
