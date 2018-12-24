namespace Tests

open NUnit.Framework

[<TestClass>]
type TestClass () =

    [<SetUp>]
    member this.Setup () =
        ()

    [<Test>]
    member this.Day1Part1 () =
        let file = "../../../../../../code/advent/input1.txt"
        let lines = System.IO.File.ReadLines(file)
        let convert str = 
            printfn "%s" str |> ignore
            int str
        let numbers = lines |> Seq.map convert
        let result : int = Seq.sum numbers
        Assert.AreEqual(0, result)
        
    [<Test>]
    member this.Day1Part2 () =
        let file = "../../../../../../code/advent/input1.txt"
        let lines = Seq.toList (System.IO.File.ReadLines(file))
        let mutable set = Set.empty
        let update (dict : Set<System.IComparable>) (curr : int) = 
            if (dict.Contains curr)
                then
                    raise (new System.Exception(curr.ToString())) |> ignore
                    (dict, curr)
                else 
                    ((dict.Add curr), curr)
        let numbers = lines |> Seq.map int
        let stuff vals (dict, curr) = Seq.fold (fun diff -> update dict curr + diff) vals
        let rec infi vals state = infi vals (stuff vals state)
        //infi numbers 0
        infi numbers (Set.empty, 0) //|> ignore
