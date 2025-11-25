module Dec06Parser

// F#
// Required: FParsec package
open FParsec

// Define a record type for a command
type LedCommand =
    | TurnOn of int * int * int * int
    | TurnOff of int * int * int * int
    | Toggle of int * int * int * int

// Parser for an integer
let pint: Parser<int, unit> = pint32 .>> spaces

// Parser for coordinates in the form x,y
let pcoord: Parser<int*int, unit> =
    pipe2 (pint .>> pchar ',' ) pint (fun x y -> (x,y))

// Parser for the "turn on" command: turn on x0,y0 through x1,y1
let pTurnOn: Parser<LedCommand, unit> =
    stringReturn "turn on " () >>. pcoord .>> pstring "through " .>>. pcoord
    |>> fun ((x0,y0),(x1,y1)) -> TurnOn(x0,y0,x1,y1)

// Parser for the "turn on" command: turn on x0,y0 through x1,y1
let pTurnOff: Parser<LedCommand, unit> =
    stringReturn "turn off " () >>. pcoord .>> pstring "through " .>>. pcoord
    |>> fun ((x0,y0),(x1,y1)) -> TurnOff(x0,y0,x1,y1)

// Parser for the "turn on" command: turn on x0,y0 through x1,y1
let pToggle: Parser<LedCommand, unit> =
    stringReturn "toggle " () >>. pcoord .>> pstring "through " .>>. pcoord
    |>> fun ((x0,y0),(x1,y1)) -> Toggle(x0,y0,x1,y1)

// Combined parser for any LED command
let pLedCommand = pTurnOn <|> pTurnOff <|> pToggle

// Function to actually simulate turning on LEDs on a grid
let executeCommand fTurnOn fTurnOff fToggle grid cmd =
    match cmd with
    | TurnOn(x0,y0,x1,y1) ->
        for x in x0..x1 do
            for y in y0..y1 do
                if x < Array2D.length1 grid && y < Array2D.length2 grid then
                    fTurnOn grid (x,y)
    | TurnOff(x0,y0,x1,y1) ->
        for x in x0..x1 do
            for y in y0..y1 do
                if x < Array2D.length1 grid && y < Array2D.length2 grid then
                    fTurnOff grid (x,y)
    | Toggle(x0,y0,x1,y1) ->
        for x in x0..x1 do
            for y in y0..y1 do
                if x < Array2D.length1 grid && y < Array2D.length2 grid then
                    fToggle grid (x,y)

let turnOn1 (gr:_[,]) (x,y) = gr[x,y] <- true
let turnOff1 (gr:_[,]) (x,y) = gr[x,y] <- false
let turnOn2 (gr:_[,]) (x,y) = gr[x,y] <- gr[x,y] + 1
let turnOff2 (gr:_[,]) (x,y) =
    if gr.[x,y] > 0 then
        gr.[x,y] <- gr.[x,y] - 1
let toggle1 (gr:_[,]) (x,y) = gr[x,y] <- not gr[x,y]
let toggle2 (gr:_[,]) (x,y) = gr[x,y] <- gr[x,y] + 2

let executeCommand1 = executeCommand turnOn1 turnOff1 toggle1
let executeCommand2 = executeCommand turnOn2 turnOff2 toggle2

let printGrid (grid: bool[,]) =
    for y in 0..(Array2D.length2 grid)-1 do
        for x in 0..(Array2D.length1 grid)-1 do
            printf "%s " (if grid[x,y] then "1" else ".")
        printfn ""


let tryParser () =
    let inputs = [
        "turn on 0,0 through 7,7"
        "turn off 3,3 through 4,4"
        "toggle 0,0 through 5,5"
    ]
    
    let world = Array2D.create 10 10 false

    for input in inputs do
        match run pLedCommand input with
        | Success(result, _, _) ->
            executeCommand1 world result
            printfn "Parsing '%s' -> %A" input result
        | Failure(err, _, _) ->
            printfn "Parsing '%s' failed: %s" input err
    printGrid world
    
let runCommandsFromInput fCommand (world:_[,]) (inputs:string list) =
    for input in inputs do
        match run pLedCommand input with
        | Success(result, _, _) ->
            fCommand world result
        | Failure(err, _, _) ->
            printfn "Parsing '%s' failed: %s" input err
    
let runCommandsFromInput1 = runCommandsFromInput executeCommand1
let runCommandsFromInput2 = runCommandsFromInput executeCommand2