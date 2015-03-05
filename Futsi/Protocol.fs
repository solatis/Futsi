namespace Futsi

open System.Net.Sockets
open System.IO

open FParsec.CharParsers
open Parser
open Ast

exception ProtocolException of string

module Protocol = 

    // The first two commands of a response should always be in a specific
    // order, and the rest of the response contains the actual data we are
    // looking for.
    //
    // This function parses the two first commands/keywords, and returns
    // the remainders of the tokens.
    let expectResponse (k1 : string, k2 : string) (reader: StreamReader) : Token list = 
        let tokens = runStream Parser.line reader
        match tokens with
        | (k1, None) :: (k2, None) :: xs -> xs
        | _                              -> raise (ProtocolException ("Invalid response: " + tokens.ToString()))

    // Connects to host and executes within a callback. Closes connection
    // once execution completes.
    let connect host port callback : 'a = 
        use client = new TcpClient (host, port)
        use stream = client.GetStream()
        use reader = new StreamReader(stream)
        use writer = new StreamWriter(stream)

        callback reader writer

    // Negotiates a specific protocol version with the remote
    let versionWithConstraint (min,max) (reader: StreamReader) (writer : StreamWriter) : int list =
        // Converts an int array [3;1] to the string "3.1"
        let versionToString version =
            String.concat "." (List.map string version)

        // Generates the string to announce ourselves with the SAM bridge
        let helloString : string = 
            List.reduce (+) 
                ["HELLO VERSION ";
                 "MIN=" + versionToString min + " ";
                 "MAX=" + versionToString max]

        // Converts a string "3.1" to an int array [3;1]
        let stringToVersion (version : string) : int list = 
            List.map int (Array.toList (version.Split [|'.'|]))           

        System.Diagnostics.Debug.WriteLine ("Writing hello string: " + helloString)

        writer.WriteLine helloString
        writer.Flush()

        System.Diagnostics.Debug.WriteLine ("Written line")

        let res = expectResponse ("HELLO", "VERSION") reader 

        match value "RESULT" res with
        | Some("OK") -> stringToVersion (value "VERSION" res |> Option.get)
        | _          -> raise(ProtocolException("Unrecognized result: " + res.ToString()))

    // Default implementation of version negotiation, defaults to version 3.1
    let version : (StreamReader -> StreamWriter -> int list) = 
        versionWithConstraint ([3;1],[3;1])