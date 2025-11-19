module Dec05

open System
open System.IO

let day = 5

MetaUtils.getTodayInput day
let filename = MetaUtils.todayFilename day

let inputStrings =
    File.ReadAllLines filename

let isVowel (c:char) =
    "aeiou".Contains c

let has3vowels (s:string) =
    s |> Seq.filter isVowel |> Seq.length > 2
    
let hasDoubles (s:string) =
    s
    |> Seq.pairwise
    |> Seq.filter (fun (a,b) -> a=b)
    |> Seq.length > 0
    
let noForbiddenPairs (s:string) =
    let forbiddenPairs = ["ab";"cd";"pq";"xy"]
    s
    |> Seq.mapi (fun i x ->
        if i+1<s.Length then
            new String([|x;s[i+1]|])
        else
            ""
        )
    |>Seq.filter (fun p -> forbiddenPairs |> Seq.contains p)
    |>Seq.isEmpty

let isGood (x:string) =
    (has3vowels x)
    && (hasDoubles x)
    && (noForbiddenPairs x)
    
let isgoodPrint (x:string) =
    printf "%s " x
    if (isGood x ) then
        printfn "good"
    else
        printfn "naughty"
let Part1 () =
    // "ugknbfddgicrmopn" |> isgoodPrint
    // "aaa" |> isgoodPrint
    // "jchzalrnumimnmhp" |> isgoodPrint
    "haegwjzuvuyypxyu" |> isgoodPrint
    inputStrings
    |> Seq.filter isGood
    |> Seq.length
    |> printfn "%d"


let hasGoodTriples (x:string) =
    seq {
        for i=0 to x.Length-3 do
            if x[i] = x[i+2] then
                yield true
    }
    |> Seq.length > 0
        
let hasTwoPairs (x:string) =
    seq {
        for i=0 to x.Length-2 do
            yield (String(x[i..i+1]), i)
    }
    |> Seq.groupBy fst
    |> Seq.map (fun (y,pairs) ->
        
        let s = pairs |> Seq.map snd//only look at start index of the pair
        
        let isRepeatedPair =
            Seq.allPairs s s
            |> Seq.filter (fun (a,b)-> b-a > 1) //must not overlap, i.e. start indices differ by at least 2
            |> Seq.isEmpty
            |> not
        if isRepeatedPair then
            printfn "repeated pair %s" y
        isRepeatedPair
        )
    |> Seq.filter id
    |> Seq.isEmpty
    |> not

let isNice2 x =
    hasGoodTriples x && hasTwoPairs x
    
let isgoodPrint2 (x:string) =
    printf "%s " x
    if (isNice2 x ) then
        printfn "good"
    else
        printfn "naughty"
        
let Part2() =
    // printf "nice "
    // printfn "%A" (isgoodPrint2 "qjhvhtzxzqqjkmpb")
    // printf "nice "
    // printfn "%A" (isgoodPrint2 "xxyxx")
    // printf "naughty "
    // printfn "%A" (isgoodPrint2 "uurcxstgmygtbstg")
    // printf "naughty "
    // printfn "%A" (isgoodPrint2 "ieodomkazucvgmuy")
    //     
    inputStrings
    |> Seq.filter isNice2
    |> Seq.length
    |> printfn "%d"
    