namespace Tests

open System
open NUnit.Framework

type Tree = Node of Tree list * int list

module Day8 =
    let parseTree (text : string) =
        let ints = text.Split [|' '|] |> Array.map int |> Array.toList
        let rec readNodes (list : int list) : (Tree * int list) =
            match list with 
            | 0 :: numMeta :: xs -> (Node ([], List.take numMeta xs), List.skip numMeta xs)
            | numNodes :: numMeta :: xs -> 
                let (tree, rem) = readNodes xs
                let (nextTree, nextRem) = readNodes ((numNodes - 1) :: numMeta :: rem)
                let (Node (subTree, md)) = nextTree
                (Node (tree :: subTree, md), nextRem)
                //(List.concat [tree; nextTree], nextRem) //Puts all on top level
            | xs -> ((Node ([], [])), xs)
        let (parsed, _) = readNodes ints
        parsed
        
        
    let rec sumMetadata (tree : Tree) = 
        match tree with 
        | Node (treeList, meta) -> List.sum (List.map sumMetadata treeList) + (List.sum meta)
        
    let rec value (tree : Tree) =
        match tree with 
        | Node ([], meta) -> List.sum meta
        | Node (children, refs) -> 
            let valueOfChild ref = 
                let child : Tree option =
                    if children.Length >= ref
                    then 
                        Some (children.[ref - 1])
                    else 
                        None
                match child with 
                | Some ct -> value ct
                | None -> 0
            List.sum (List.map valueOfChild refs)
        

[<TestClass>]
type Day8 () =
    [<Test>]
    member this.Part1() =
        let file = "../../../../../../code/advent/input8.txt"
        let text = System.IO.File.ReadAllText(file)
        let trees = Day8.parseTree text
        let sum = Day8.sumMetadata (trees)
        Assert.AreEqual(36027, sum)
        
    [<Test>]
    member this.Part1example() =
        let text = "2 3 0 3 10 11 12 1 1 0 1 99 2 1 1 2"
        let trees = Day8.parseTree text
        let sum = Day8.sumMetadata (trees)
        Assert.AreEqual(138, sum)
        
    [<Test>]
    member this.Part2() =
        let file = "../../../../../../code/advent/input8.txt"
        let text = System.IO.File.ReadAllText(file)
        let trees = Day8.parseTree text
        let value = Day8.value (trees)
        Assert.AreEqual(23960, value)
        
    [<Test>]
    member this.Part2example() =
        let text = "2 3 0 3 10 11 12 1 1 0 1 99 2 1 1 2"
        let trees = Day8.parseTree text
        let value = Day8.value (trees)
        Assert.AreEqual(66, value)

