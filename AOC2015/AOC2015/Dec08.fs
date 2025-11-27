module Dec08

open System
open System.IO
open Dec08Parser

let day = 8

MetaUtils.getTodayInput day
let filename = MetaUtils.todayFilename day

let inputStrings =
    File.ReadAllLines filename

let Part1 () =
    //inputStrings    |> Seq.iter (printfn "%s")
    // inputStrings
    // |> Seq.iter (Dec08Parser.test stringLiteral)
    
    let originalLength =
        inputStrings
        |> Seq.map (fun x-> (Dec08Parser.parseLine stringLiteral x).Length)
        |> Seq.sum

    let encodedLength =
        inputStrings
        |> Seq.map _.Length
        |> Seq.sum
    encodedLength-originalLength
    |> printfn "%d"

let encodeString (s:string) =
    seq{
        yield '\"'
        for c in s do
            match c with
            | '"' ->
                yield '\\'
                yield '\"'
            | '\\' ->
                yield '\\'
                yield '\\'
            | _ -> yield c
        yield '\"'
    }
    
let testEncode(s:string) =
    new string (Array.ofSeq(encodeString s))  
    |> printfn "%s"
    
let Part2 () =
    testEncode "\"\""
    testEncode "\"abc\""
    testEncode "\"aaa\\\"aaa\""
    
    let originalLength=
        inputStrings
        |> Seq.map _.Length
        |> Seq.sum
        
    let encodedLength =
        inputStrings
        |> Seq.collect encodeString
        |> Seq.length
        
    printfn "%d" (encodedLength-originalLength)
        
