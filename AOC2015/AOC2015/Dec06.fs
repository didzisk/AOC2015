module Dec06

open System
open System.IO

let day = 6

MetaUtils.getTodayInput day
let filename = MetaUtils.todayFilename day

let inputStrings =
    File.ReadAllLines filename
    |> List.ofArray

let Part1 () =
    let world = Array2D.create 1000 1000 false
    Dec06Parser.runCommandsFromInput1 world inputStrings
    
    world
    |> Array2D.map (fun x-> if x then 1 else 0)
    |> Seq.cast<int>
    |> Seq.sum |> printfn "%d"
    
let Part2 () =
    let world = Array2D.create 1000 1000 0
    Dec06Parser.runCommandsFromInput2 world inputStrings
    
    world
    |> Seq.cast<int>
    |> Seq.sum |> printfn "%d"