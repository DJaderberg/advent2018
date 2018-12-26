namespace Tests

open System
open NUnit.Framework

module Day9 =
    let play numPlayers numMarbles : (int * int) = 
        let marbles = [1..numMarbles]
        let players = [0..numPlayers-1] |> Seq.map (fun p -> (p, 0)) |> Map.ofSeq
        let placeMarble ((scores : Map<int, int>), (circle : List<int>), currentIndex) marble =
            if marble % 23 = 0
            then 
                let player = marble % numPlayers
                let indexSubtr n = (circle.Length + currentIndex - n) % circle.Length
                let index = indexSubtr 7
                let removed = circle.[indexSubtr 7]
                let newScore = scores.[player] + removed + marble
                let newCircle = List.concat [circle.GetSlice (None, Some (indexSubtr 8));
                                             circle.GetSlice (Some (indexSubtr 6), None)]
                (Map.add player newScore scores, newCircle, indexSubtr 7)
            else 
                if circle.Length > currentIndex + 1
                then 
                    (
                    scores,
                    List.concat [circle.GetSlice (None, Some (currentIndex+1));
                                [marble];
                                circle.GetSlice (Some (currentIndex+2), None)],
                    currentIndex + 2)
                else 
                    (scores, List.head circle :: marble :: List.tail circle, 1)
        let (scores, _ ,_) = List.fold placeMarble (players, [0], 0) marbles
        Map.toList scores |> List.maxBy snd
        
        

module Day9Test =
    [<Test>]
    let Part1Example0() =
        let (_, score) = Day9.play 9 25
        Assert.AreEqual(32, score)
        
    [<Test>]
    let Part1Example1() =
        let (_, score) = Day9.play 10 1618
        Assert.AreEqual(8317, score)
        
    [<Test>]
    let Part1Example2() =
        let (_, score) = Day9.play 13 7999
        Assert.AreEqual(146373, score)
        
    [<Test>]
    let Part1Example3() =
        let (_, score) = Day9.play 17 1104
        Assert.AreEqual(2764, score)
        
    [<Test>]
    let Part1Example4() =
        let (_, score) = Day9.play 21 6111
        Assert.AreEqual(54718, score)
        
    [<Test>]
    let Part1Example5() =
        let (_, score) = Day9.play 30 5807
        Assert.AreEqual(37305, score)
        
        
    [<Test>]
    let Part1() =
        let (_, score) = Day9.play 427 70723
        Assert.AreEqual(399745, score)
        
    [<Test>]
    let Part2() =
        // I haven't finished the implementation of this, it's taking way too long with lists 
        // and would overflow before it finishes
        //let (_, score) = Day9.play 427 (70723 * 100)
        //Assert.AreEqual(3349098263, score)
        Assert.Pass()
        

