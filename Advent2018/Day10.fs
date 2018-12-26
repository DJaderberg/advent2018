namespace Tests

open System
open NUnit.Framework
open System.Text.RegularExpressions

module Day10 =
    let regex = new Regex("position=<\s*(?<posX>-?\d+),\s*(?<posY>-?\d+)>\s*velocity=<\s*(?<velX>-?\d+),\s*(?<velY>-?\d+)>")
    let getGroup (name : string) (expMatch : Match) = expMatch.Groups.[name].ToString()
    let parseLine line = line |> regex.Match |> (fun m -> ((int (getGroup "posX" m), int (getGroup "posY" m)),(int (getGroup "velX" m), int (getGroup "velY" m))))
    let lineAt ((x,y),(vx, vy)) t = (x+vx*t, y+vy*t)
    let measure (positions : (int * int) list) =
        let minX = List.map fst positions |> List.min
        let maxX = List.map fst positions |> List.max
        let minY = List.map snd positions |> List.min
        let maxY = List.map snd positions |> List.max
        maxX-minX + maxY-minY
    let chooseRange eval min max =
        let mid = min + (max - min)/2
        let [a;b;c] = [min;mid;max] |> List.map eval |> List.map measure
        if a+b < b+c
        then
            (min, mid)
        else
            (mid, max)
            
    let rec iterate eval startMin startMax =
        let (min, max) = chooseRange eval startMin startMax
        if max - min <= 1 then 
            (min, max)
        else 
            iterate eval min max
        
        

module Day10Test =
    let example = """position=< 9,  1> velocity=< 0,  2>
position=< 7,  0> velocity=<-1,  0>
position=< 3, -2> velocity=<-1,  1>
position=< 6, 10> velocity=<-2, -1>
position=< 2, -4> velocity=< 2,  2>
position=<-6, 10> velocity=< 2, -2>
position=< 1,  8> velocity=< 1, -1>
position=< 1,  7> velocity=< 1,  0>
position=<-3, 11> velocity=< 1, -2>
position=< 7,  6> velocity=<-1, -1>
position=<-2,  3> velocity=< 1,  0>
position=<-4,  3> velocity=< 2,  0>
position=<10, -3> velocity=<-1,  1>
position=< 5, 11> velocity=< 1, -2>
position=< 4,  7> velocity=< 0, -1>
position=< 8, -2> velocity=< 0,  1>
position=<15,  0> velocity=<-2,  0>
position=< 1,  6> velocity=< 1,  0>
position=< 8,  9> velocity=< 0, -1>
position=< 3,  3> velocity=<-1,  1>
position=< 0,  5> velocity=< 0, -1>
position=<-2,  2> velocity=< 2,  0>
position=< 5, -2> velocity=< 1,  2>
position=< 1,  4> velocity=< 2,  1>
position=<-2,  7> velocity=< 2, -2>
position=< 3,  6> velocity=<-1, -1>
position=< 5,  0> velocity=< 1,  0>
position=<-6,  0> velocity=< 2,  0>
position=< 5,  9> velocity=< 1, -2>
position=<14,  7> velocity=<-2,  0>
position=<-3,  6> velocity=< 2, -1>"""

    [<Test>]
    let Part1Example() =
        let lines = example.Split '\n'
        let parsed = Array.map Day10.parseLine lines
        let evals = parsed |> Array.map Day10.lineAt
        let eval x = Array.map (fun f -> f x) evals |> Array.toList
        let (min, max) = Day10.iterate eval 0 10
        Assert.AreEqual(3, max)
        
    [<Test>]
    let BothParts() =
        let file = "../../../../../../code/advent/input10.txt"
        let lines = System.IO.File.ReadLines(file)
        let parsed = Seq.map Day10.parseLine lines
        let evals = parsed |> Seq.map Day10.lineAt
        let eval x = Seq.map (fun f -> f x) evals |> Seq.toList
        let (min, max) = Day10.iterate eval 0 100000
        let res = eval max |> List.map (fun (x,y) -> (y,x))
        let xmin = res |> List.map fst |> List.min 
        let xmax = res |> List.map fst |> List.max
        let ymin = res |> List.map snd |> List.min 
        let ymax = res |> List.map snd |> List.max
        let (xrange, yrange) = ([xmin..xmax],[ymin..ymax])
        let chooseChar x y = 
            if List.contains (x,y) res
            then '#'
            else ' '
        let str =
            let printLine x = yrange |> List.map (chooseChar x) |> System.String.Concat
            xrange |> List.map printLine |> String.concat "\n"
        printfn "%s" str
        printfn "Correct answer: XLZAKBGZ"
        Assert.AreEqual(10656, max)
