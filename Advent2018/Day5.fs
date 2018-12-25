namespace Tests

open System
open System.Linq
open NUnit.Framework

module Day5 =
    let rec reduce text  =
        match text with 
        | a :: b :: xs -> if a <> b && System.Char.ToUpper a = System.Char.ToUpper b then reduce xs else a :: reduce (b :: xs)
        | xs -> xs
    let rec topReduce chars =
        let reduced = reduce chars
        let length = List.length reduced
        if reduced = chars
        then 
            reduced
        else 
            topReduce reduced
            
[<TestClass>]
type Day5 () =
    [<Test>]
    member this.Part1() =
        let file = "../../../../../../code/advent/input5.txt"
        let text = System.IO.File.ReadAllText(file).ToCharArray() |> Array.toList
        let res = Day5.topReduce text
        let length = List.length res
        raise (new Exception((length-1).ToString())) |> ignore
        
    [<Test>]
    member this.Part2() =
        let file = "../../../../../../code/advent/input5.txt"
        let text = System.IO.File.ReadAllText(file).ToCharArray() |> Array.toList
        let lower = ['a'..'z']
        let removeUL char =
            let upper = System.Char.ToUpper char
            let shouldKeep cur = (cur <> char && cur <> upper)
            let filt = List.filter shouldKeep text
            filt
        let filtered = Seq.map (removeUL) lower
        let reduced = Seq.map Day5.topReduce filtered
        let shortest = Seq.minBy List.length reduced
        let length = List.length shortest
        raise (new Exception((length-1).ToString())) |> ignore
        
