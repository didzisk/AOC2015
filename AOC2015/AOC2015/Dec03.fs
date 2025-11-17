module Dec03

open System.IO

let day = 3

MetaUtils.getTodayInput day
let filename = MetaUtils.todayFilename day

let inputString = File.ReadAllText filename

let directions =
    seq{
        ('^', (0,1))
        ('>',(1,0))
        ('v',(0,-1))
        ('<',(-1,0))
    }
    |> Map.ofSeq

let folder (x,y) dir =
    let dx,dy = directions[dir]
    x+dx,y+dy
    
let takeEveryOther list = 
    list 
    |> Seq.mapi (fun index item -> (item, index))
    |> Seq.filter (fun (item, index) -> index % 2 = 0)
    |> Seq.map fst



let Part1 () =
//    printfn $"%s{inputString}"
    
    printfn "%A" directions
    
    inputString
    |> Seq.scan folder (0,0)
    |> Set.ofSeq
    |> Set.count
    |> printfn "%d"
    
let Part2 () =
    seq{
        yield! inputString
        |> takeEveryOther
        |> Seq.scan folder (0,0)
    
        yield! inputString
        |> Seq.skip 1
        |> takeEveryOther
        |> Seq.scan folder (0,0)
    }
    |> Set.ofSeq
    |> Set.count
    |> printfn "%d" 