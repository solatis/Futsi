﻿namespace Futsi

module Ast = 
    // A Token is a combination of a key and possibly an associated value
    type Token =  string * string option

    // A Line is just a list of tokens; the parser separates based on newlines.
    type Line  = Token list
        
    // Returns true if key is part of token list.
    let rec key (k1:string) (tokens:Token list) : bool = 
        match tokens with
        | []                        -> false
        | (k2, _) :: _ when k1 = k2 -> true
        | _       :: xs             -> key k1 xs

    // Returns value if key is found, None otherwise.
    let rec value (k1:string) (tokens:Token list) : string option = 
        match tokens with
        | []                        -> None
        | (k2, v) :: _ when k1 = k2 -> v
        | _       :: xs             -> value k1 xs

