namespace Tests

open System
open NUnit.Framework

module Day11 =
    
    let hundredsDigit (str : string) =
        let chars = str.ToCharArray()
        if chars.Length >= 3
        then int (chars.[chars.Length-3].ToString())
        else 0
    
    let powerLevel serial y x =
        let rackId = x + 10
        let str =
            rackId
            |> (*) y
            |> (+) serial
        str
        |> (*) rackId 
        |> (fun z -> z.ToString())
        |> hundredsDigit
        |> (fun z -> z - 5)
        
    let powerLevels serial =
        let xrange = [1..300]
        let yrange = [1..300]
        let pl y = List.map (fun x -> (x, powerLevel serial y x)) xrange
        let levelY y = pl y |> List.map (fun (x, level) -> ((x,y),level))
        List.map levelY yrange |> List.concat |> Map.ofList
    
    let totalPower (powerLevels : Map<(int * int),int>) (x,y) =
        let tups = [(x,y);(x+1,y);(x+2,y);(x,y+1);(x+1,y+1);(x+2,y+1);(x,y+2);(x+1,y+2);(x+2,y+2)]
        if x > 298 || y > 298
        then Int32.MinValue
        else Seq.map (fun v -> powerLevels.Item(v)) tups |> Seq.sum
        
    let largestPower powerLevels =
        let zoneSum = totalPower powerLevels
        let tups =
            List.map (fun x -> List.zip (List.replicate 298 x) [1..298]) [1..298]
            |> List.concat
            |> List.map (fun tup -> (zoneSum tup, tup))
        List.maxBy fst tups
        

module Day11Test =
    [<Test>]
    let Part1() =
        let serial = 8772
        let powerLevels = Day11.powerLevels serial
        let res = snd (Day11.largestPower powerLevels)
        Assert.AreEqual((235,31), res)

    [<Test>]
    let PowerLevel0() =
        let level = Day11.powerLevel 8 5 3
        Assert.AreEqual(4, level)
        
    [<Test>]
    let PowerLevel1() =
        let level = Day11.powerLevel 57 79 122 
        Assert.AreEqual(-5, level)

    [<Test>]
    let PowerLevel2() =
        let level = Day11.powerLevel 39 196 217 
        Assert.AreEqual(0, level)
        
    [<Test>]
    let PowerLevel3() =
        let level = Day11.powerLevel 71 153 101 
        Assert.AreEqual(4, level)
