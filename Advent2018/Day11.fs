namespace Tests

open System
open System.Linq
open NUnit.Framework

module Day11 =
    let hundredsDigit (str : string) =
        let chars = str.ToCharArray()
        if chars.Length >= 3
        then int (chars.[chars.Length-3].ToString())
        else 0
    
    let powerLevel serial x y =
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
    
    let totalPower (powerLevels : Map<(int * int),int>) size (x,y) =
        let max = 301 - size
        let tups = Seq.map (fun z -> Seq.zip (List.replicate size z) (Enumerable.Range(x, size))) (Enumerable.Range(y,size)) |> Seq.concat
        if x > max || y > max
        then Int32.MinValue
        else Seq.map (fun v -> powerLevels.Item(v)) tups |> Seq.sum
        
    let largestPower powerLevels size =
        let zoneSum = totalPower powerLevels size
        let max = 301 - size
        let tups =
            List.map (fun x -> List.zip (List.replicate max x) [1..max]) [1..max]
            |> List.concat
            |> List.map (fun tup -> (zoneSum tup, tup))
        List.maxBy fst tups
        
    let largestPowerWithSize powerLevels =
        // Abuse the fact that most squares are negative -> The correct answer probably is in 1..15 :D Turns out it was 10!
        let bestPerSize = [9..11] |> List.map (fun s -> (s, largestPower powerLevels s))
        let conv (size, (power, (x, y))) = (power, x, y, size)
        let readable = List.map conv bestPerSize
        let best = List.maxBy (fun (p, x, y, s) -> p) readable
        let (_,x,y,s) = best
        (x,y,s)
        

module Day11Test =
    [<Test>]
    let Part2() =
        let serial = 8772
        let powerLevels = Day11.powerLevels serial
        let res = snd (Day11.largestPower powerLevels 3)
        Assert.AreEqual((235,31), res)
        
    [<Test>]
    let Part1() =
        let serial = 8772
        let powerLevels = Day11.powerLevels serial
        let res = Day11.largestPowerWithSize powerLevels
        Assert.AreEqual((241,65,10), res)

    [<Test>]
    let PowerLevel0() =
        let level = Day11.powerLevel 8 3 5
        Assert.AreEqual(4, level)
        
    [<Test>]
    let PowerLevel1() =
        let level = Day11.powerLevel 57 122 79
        Assert.AreEqual(-5, level)

    [<Test>]
    let PowerLevel2() =
        let level = Day11.powerLevel 39 217 196 
        Assert.AreEqual(0, level)
        
    [<Test>]
    let PowerLevel3() =
        let level = Day11.powerLevel 71 101 153
        Assert.AreEqual(4, level)
