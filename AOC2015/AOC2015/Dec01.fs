module Dec01

open System.IO

let day = 1

MetaUtils.getTodayInput day
let filename = MetaUtils.todayFilename day

let inputString = File.ReadAllText filename

let Part1 () =
    printfn $"%s{inputString}"
    
    inputString
    |> Seq.map (fun x->
        match x with
        | '(' -> 1
        | _ -> -1)
    |> Seq.sum
    |> printfn "%d"
    
let Part2 () =
    let folder n x =
        match x with
        | '(' -> n+1
        | _ -> n-1
 
    
    inputString
    |> Seq.scan folder 0
    |> Seq.findIndex (fun x->x<0)
    |> printfn "%d"
