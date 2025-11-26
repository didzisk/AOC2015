module Dec07

open System
open System.IO
open Dec07Parser

let day = 7

MetaUtils.getTodayInput day
let filename = MetaUtils.todayFilename day

let inputStrings =
    File.ReadAllLines filename
    
let tryResolveWire (world:Map<string,WireCommand>) (w:Wire) =
        match w with
        | Signal _ -> w
        | WireId s ->
            match world[s] with
            | Wire(Signal(a)) -> Signal a
            | _ -> w

let resolveCommand (world:Map<string,WireCommand>) (c:WireCommand) =
    match c with
    | Wire wire ->
        match wire with
        | Signal _ -> c
        | WireId s -> world[s]
    | NotCommand wire ->
        match tryResolveWire world wire with
        | Signal i -> Wire(Signal(~~~i))
        | WireId _ -> c
    | AndCommand(wire, wire1) ->
        match (tryResolveWire world wire, tryResolveWire world wire1) with
        | Signal(a), Signal(b) -> Wire(Signal(a &&& b))
        | _ -> c
    | OrCommand(wire, wire1) -> 
        match (tryResolveWire world wire, tryResolveWire world wire1) with
        | Signal(a), Signal(b) -> Wire(Signal(a ||| b))
        | _ -> c
    | LShiftCommand(wire, wire1) ->
        match (tryResolveWire world wire, tryResolveWire world wire1) with
        | Signal(a), Signal(b) -> Wire(Signal(a <<< b))
        | _ -> c
    | RShiftCommand(wire, wire1) ->
        match (tryResolveWire world wire, tryResolveWire world wire1) with
        | Signal(a), Signal(b) -> Wire(Signal(a >>> b))
        | _ -> c
        
let rec solve1 (world:Map<string,WireCommand>) =
    // world
    // |> Seq.iter (printfn "%A")

    let newWorld =
        world
        |> Map.map (fun _ -> resolveCommand world)

    printfn "*"
    
    match newWorld["a"] with
    | Wire(Signal i) ->
        printfn "%d ***************************" i
        newWorld
    | _ ->
        solve1 newWorld

let Part1 () =
    //inputStrings    |> Seq.iter (printfn "%s")
    
    //Dec07Parser.tryPNotAnd ()
    let world =
        inputStrings
        |> parseInput
    world
    |> Seq.iter (printfn "%A")

    printfn ""
    
    let newWorld = solve1 world
    newWorld["a"]

let Part2 () =
    let a = Part1()
    let world =
        inputStrings
        |> parseInput
        
        |> Map.map (fun s w -> if s="b" then a else w)
    solve1 world
     