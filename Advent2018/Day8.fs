namespace Tests

open System
open NUnit.Framework
open System.Text.RegularExpressions

type Tree = Node of Tree list * int list

module Day8 =
    let parseTree (text : string) =
        let ints = text.Split [|' '|] |> Array.map int |> Array.toList
        let rec readNodes ((tree : Tree list), list) _ =
            match list with 
            | numNodes :: numMeta :: xs -> 
                let (nodes, remXs) = 
                    List.replicate numNodes ()
                    |> List.fold readNodes (tree, xs)
                let meta = List.take numMeta remXs
                ([Node (nodes, meta)], List.skip numMeta remXs)
            | [] -> ([], [])
            | _ -> raise (new Exception("Parse error!"))
        let (parsed, []) = readNodes ([], ints) ()
        List.head parsed
        
        
    let rec sumMetadata (tree : Tree) = 
        match tree with 
        | Node (treeList, meta) -> List.sum (List.map sumMetadata treeList) + (List.sum meta)
            
        

[<TestClass>]
type Day8 () =
    [<Test>]
    member this.Part1() =
        let file = "../../../../../../code/advent/input8.txt"
        let text = System.IO.File.ReadAllText(file)
        let trees = Day8.parseTree text
        let sum = Day8.sumMetadata trees
        Assert.AreEqual(36027, sum)
        
    [<Test>]
    member this.Part1example() =
        let text = "2 3 0 3 10 11 12 1 1 0 1 99 2 1 1 2"
        let trees = Day8.parseTree text
        let sum = Day8.sumMetadata trees
        Assert.AreEqual(138, sum)

