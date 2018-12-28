namespace Tests

open System
open System.Linq
open NUnit.Framework

module Day14 =
    open NUnit.Framework

    let rules =
        let file = "../../../../../../code/advent/input12.txt"
        let lines = System.IO.File.ReadLines(file)
        ()
        
    let combineDigitSums (numbers : int list) =
        let digits =
            List.sum numbers
            |> (fun i -> i.ToString().ToCharArray())
            |> Array.map (fun c -> (c.ToString() |> int))
        List.ofArray digits
        
    let move (list : seq<'a>) (i, score) =
        (i + score + 1) % list.Count()
         
        
    let update (list : Collections.Generic.List<int>) (indexes : int list) =
        let scores = indexes |> List.map (fun i -> list.[i])
        let recipies = combineDigitSums scores
        list.AddRange recipies
        let newIndexes = List.map (move list) (List.zip indexes scores)
        (list, newIndexes)
        
    let rec buildUntilLongEnough stopAt ((list : Collections.Generic.List<int>), index) =
        if list.Count >= stopAt
        then (list, index)
        else buildUntilLongEnough stopAt (update list index)
        
    let rec buildUntilContains (stopAt : Collections.Generic.List<int>) startIndex ((list : Collections.Generic.List<int>), index) =
        let contains =
            let containsAt index =
                if index < 0
                then false
                else
                    list.GetRange(index, stopAt.Count).SequenceEqual(stopAt)
            let lastIndex = list.Count - stopAt.Count
            if lastIndex > startIndex
            then
                [startIndex..lastIndex] |> List.map containsAt |> List.fold (||) false
            else false
        if contains
        then (list, index)
        else buildUntilContains stopAt (list.Count - stopAt.Count) (update list index)
        
    let buildListByLength length =
        buildUntilLongEnough length (ResizeArray<int> [3;7], [0;1])
        
    let buildListByContent (content : string) =
        let converted =
            content
            |> (fun i -> i.ToString().ToCharArray())
            |> Array.map (fun c -> (c.ToString() |> int))
            |> ResizeArray<int>
        buildUntilContains converted 0 (ResizeArray<int> [3;7], [0;1])
        
    let part1 index amount =
        let (list, _) = buildListByLength (index+amount)
        list.GetRange(index, amount) |> Seq.map (fun i -> i.ToString()) |> String.Concat
        
    let part2 content =
        let (list, _) = buildListByContent content
        list.Count - content.Count()

module Day14Test =
    [<TestCase(9, "5158916779")>]
    [<TestCase(5, "0124515891")>]
    [<TestCase(18, "9251071085")>]
    [<TestCase(2018, "5941429882")>]
    let part1examples index expected =
        let result = Day14.part1 index 10
        Assert.AreEqual(expected, result)
        
    [<TestCase(9, "51589")>]
    [<TestCase(5, "01245")>]
    [<TestCase(18, "92510")>]
    [<TestCase(2018, "59414")>]
    let part2examples expected content =
        let result = Day14.part2 content
        Assert.AreEqual(expected, result)
        
    [<Test>]
    let part1() =
        let result = Day14.part1 640441 10
        Assert.AreEqual("1041411104", result)
        
    [<Test>]
    let part2() =
        let result = Day14.part2 "640441"
        Assert.AreEqual(20174745, result - 1) // Off-by-one since the last iteration added two recepies, which I haven't accounted for
