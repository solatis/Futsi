namespace Futsi

open FParsec
open Ast

module Parser =

    let quotedValue : Parser<string, unit> = 
        let normalCharSnippet = manySatisfy (fun c -> c <> '\\' && c <> '"')
        let escapedChar       = pstring "\\" >>. (anyOf "\\\"nrt" |>> function
                                                                      | 'n' -> "\n"
                                                                      | 'r' -> "\r"
                                                                      | 't' -> "\t"
                                                                      | c   -> string c)

        between (pstring "\"") (pstring "\"")
                (stringsSepBy normalCharSnippet escapedChar)

    let unquotedValue : Parser<string, unit> = 
        many1Chars (noneOf "\r\n\t ")

    let value : Parser<string, unit> = 
        quotedValue <|> unquotedValue

    let key : Parser<Token, unit> =
        many1Chars (noneOf "=\t ") |>> fun s -> (s, None)

    let keyValue : Parser<Token, unit> =
        (key |>> fst) .>>. (pstring "=" >>. value |>> Some)

    let token : Parser<Token, unit> = 
        spaces >>. key <|> keyValue .>> spaces

    let tokens : Parser<Token list,unit> =  
        many1 token

    let line : Parser<Token list,unit> =
        tokens .>> newline