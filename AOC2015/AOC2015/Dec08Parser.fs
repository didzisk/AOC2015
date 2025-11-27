module Dec08Parser

open System
open FParsec

let test p str =
    match run p str with
    | Success(result, _, _)   -> printfn "Success: %A" result
    | Failure(errorMsg, _, _) -> printfn "Failure: %s" errorMsg
    
let stringLiteral =
    let normalChar = satisfy (fun c -> c <> '\\' && c <> '"')
    
    let hexAsciiUnescape c =
        char (Convert.ToInt32("0x"+c, 16))
        
    let hexAscii = pstring "\\x" >>. (anyString 2 |>> hexAsciiUnescape)  
    
    let unescape c = match c with
                     | 'n' -> '\n'
                     | 'r' -> '\r'
                     | 't' -> '\t'
                     | c   -> c
    let escapedChar = pstring "\\" >>. (anyOf "\\nrt\"" |>> unescape)
    between (pstring "\"") (pstring "\"")
            (manyChars (normalChar <|> hexAscii <|> escapedChar))
            
let test1 () =
    test stringLiteral "\"ab\\x27c\"\\\\"
    test stringLiteral "\"\\\\\""
    
let parseLine p str =
    match run p str with
    | Success(result, _, _)   -> result
    | Failure(errorMsg, _, _) -> failwith (sprintf "Failure: %s" errorMsg)