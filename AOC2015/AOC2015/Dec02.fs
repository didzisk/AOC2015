module Dec02

open System.IO

let day = 2

MetaUtils.getTodayInput day
let filename = MetaUtils.todayFilename day

let inputStrings = File.ReadAllLines filename

let parseOne (s:string) =
    let x1 = s.IndexOf 'x'
    let n1 = s.Substring(0, x1) |> int
    let x2 = s.LastIndexOf 'x'
    let n2 = s.Substring(x1+1,x2-x1-1) |> int
    let n3 = s.Substring(x2+1) |> int
    let arr =
        [|n1;n2;n3|]
        |> Array.sort
    arr

let calcOne1 (arr:int array) =
    3*arr[0]*arr[1]+2*arr[0]*arr[2]+2*arr[1]*arr[2]

let calcOne2 (a:int array) =
    let p1 = (a[0]+a[1]) * 2
    let p2 = (a[0]+a[2]) * 2
    let p3 = (a[1]+a[2]) * 2
    let minP = [p1;p2;p3] |> List.min
    let vol = a[0] * a[1] * a[2]
    minP + vol

let Part1 () =
    inputStrings
    |> Seq.iter (fun x->
            printfn "%s" x
            parseOne x
            |> printfn "%A"
            )
    inputStrings
    |> Seq.map (parseOne >> calcOne1)
    |> Seq.sum
    |> printfn "%d"
    
let Part2 () =
    inputStrings
    |> Seq.iter (fun x->
            printfn "%s" x
            parseOne x
            |> printfn "%A"
            )
    inputStrings
    |> Seq.map (parseOne >> calcOne2)
    |> Seq.sum
    |> printfn "%d"        