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
        
