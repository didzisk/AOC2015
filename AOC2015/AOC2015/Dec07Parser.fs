module Dec07Parser

open FParsec
type Wire =
    | Signal of int
    | WireId of string

type WireCommand =
    | Wire of Wire
    | NotCommand of Wire
    | AndCommand of Wire * Wire
    | OrCommand of Wire * Wire
    | LShiftCommand of Wire * Wire
    | RShiftCommand of Wire * Wire

let pWireSignal = pint32 .>> spaces |>> Signal
let pWireId = many1Satisfy isAsciiLower .>> spaces |>> WireId

let pWire = pWireSignal <|> pWireId

let pNot = pstring "NOT " >>. pWire |>> NotCommand
let pAnd = pWire .>> spaces .>> pstring "AND" .>> spaces .>>. pWire |>> AndCommand
let pOr= pWire .>> spaces .>> pstring "OR" .>> spaces .>>. pWire |>> OrCommand
let pRshift= pWire .>> spaces .>> pstring "RSHIFT" .>> spaces .>>. pWire |>> RShiftCommand
let pLshift= pWire .>> spaces .>> pstring "LSHIFT" .>> spaces .>>. pWire |>> LShiftCommand

let pCommand =
    choice
        [
            pNot
            attempt pAnd
            attempt pOr
            attempt pRshift
            attempt pLshift
            attempt (pWire |>> Wire)
        ]

let pLine = pCommand .>> pstring "->" .>> spaces .>>. many1Satisfy isAsciiLower


let trypWire () =
    let inputs = [
        "aa"
        "bb"
        "128"
    ]
    

    for input in inputs do
        match run pWire input with
        | Success(result, _, _) ->
            //executeCommand1 world result
            printfn "Parsing '%s' -> %A" input result
        | Failure(err, _, _) ->
            printfn "Parsing '%s' failed: %s" input err

let tryPNotAnd () =
    let inputs = [
        "NOT aa -> a"
        "NOT bb -> a"
        "NOT 128 -> a"
        "aa AND bb -> a"
        "g OR bb -> bb"
        "lx -> a"
        "128 -> f"
    ]
    for input in inputs do
        match run pLine input with
        | Success(result, _, _) ->
            //executeCommand1 world result
            printfn "Parsing '%s' -> %A" input result
        | Failure(err, _, _) ->
            printfn "Parsing '%s' failed: %s" input err

let parseInput (lines: string array) =
    lines
    |> Seq.map (fun input -> 
        match run pLine input with
        | Success(result, _, _) ->
            //executeCommand1 world result
            printfn "Parsing '%s' -> %A" input result
            snd result, fst result
        | Failure(err, _, _) ->
            failwith (sprintf "Parsing '%s' failed: %s" input err)
            
            )
    |> Map.ofSeq