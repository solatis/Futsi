namespace Futsi

open FParsec
open Ast

module Parser =

    let horizontalSpaces : Parser<string, unit> = 
        manyChars (anyOf "\t ")

    let quotedValue : Parser<string, unit> = 
        let normalCharSnippet = manySatisfy (fun c -> c <> '\\' && c <> '"')
        let escapedChar       = pstring "\\" >>. (anyOf "\\\"nrt" |>> function
                                                                      | 'n' -> "\n"
                                                                      | 'r' -> "\r"
                                                                      | 't' -> "\t"
                                                                      | c   -> string c)

        between (pstring "\"") (pstring "\"")
                (stringsSepBy normalCharSnippet escapedChar)

        <?> "quotedValue"

    let unquotedValue : Parser<string, unit> = 
        many1Chars (noneOf "\r\n\t ")
        <?> "unquotedValue"

    let value : Parser<string, unit> = 
        quotedValue <|> unquotedValue
        <?> "value"

    let key : Parser<string, unit> =
        many1Chars (noneOf "=\r\n\t ")
        <?> "key"

    // Parses a key, with an optional value
    let parameter : Parser<Token, unit> = 
        let withValue = (pstring "=" >>. value |>> Some)

        (key .>>. (withValue <|> preturn None))
        <?> "parameter"
    
    // Parses a parameter, with optional trailing spaces
    let token : Parser<Token, unit> = 
        horizontalSpaces >>. parameter .>> horizontalSpaces
        <?> "token"

    let tokens : Parser<Token list,unit> =  
        (many1 token)
        <?> "tokens"

    let line : Parser<Token list,unit> =
        (tokens .>> newline)
        <?> "line"