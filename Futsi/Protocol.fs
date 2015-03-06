namespace Futsi

open System.Net.Sockets
open System.IO

open FParsec.CharParsers
open Parser
open Ast
open Types

module Protocol = 

    exception ProtocolException of string
    exception NoVersionException
    exception DuplicatedSessionIdException
    exception DuplicatedDestinationException
    exception InvalidKeyException

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
    let connect (HostName host) (Port port) callback : 'a = 
        use client = new TcpClient (host, port)
        use stream = client.GetStream()

        // We use a StreamReader, which has convenient methods such as ReadLine
        use reader = new StreamReader(stream)

        // And we use a StreamWriter, which has convenient methods such as WriteLine. We ensure
        // that the socket is flushed after each .Write() call to prevent programmer errors.
        use mutable writer = new StreamWriter(stream)
        writer.AutoFlush <- true

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

        System.Diagnostics.Debug.WriteLine ("Written line")

        let res = expectResponse ("HELLO", "VERSION") reader 

        match value "RESULT" res with
        | Some("OK")        -> stringToVersion (value "VERSION" res |> Option.get)
        | Some("NOVERSION") -> raise(NoVersionException)
        | _                 -> raise(ProtocolException("Unrecognized result: " + res.ToString()))

    // Default implementation of version negotiation, defaults to version 3.1
    let version : (StreamReader -> StreamWriter -> int list) = 
        versionWithConstraint ([3;1],[3;1])

    // Creates a new session
    let createSessionWith sessionId destination signatureType socketType (reader: StreamReader) (writer : StreamWriter) =         
        let sessionIdToString : string = 
            match sessionId with
            | None                -> System.Guid.NewGuid().ToString()
            | Some(SessionId(id)) -> id

        let socketTypeToString = function
            | SocketType.VirtualStream     -> "STREAM"
            | SocketType.DatagramRepliable -> "DATAGRAM"
            | SocketType.DatagramAnonymous -> "RAW"

        let destinationToString = function 
            | Some(Destination(d)), _                      -> d
            | None, None                                   -> "TRANSIENT"
            | None, Some(SignatureType.DsaSha1)            -> "TRANSIENT SIGNATURE_TYPE=DSA_SHA1"

            | None, Some(SignatureType.EcdsaSha256P256)    -> "TRANSIENT SIGNATURE_TYPE=ECDSA_SHA256_P256"
            | None, Some(SignatureType.EcdsaSha384P384)    -> "TRANSIENT SIGNATURE_TYPE=ECDSA_SHA384_P384"
            | None, Some(SignatureType.EcdsaSha512P521)    -> "TRANSIENT SIGNATURE_TYPE=ECDSA_SHA512_P521"

            | None, Some(SignatureType.RsaSha2562048)      -> "TRANSIENT SIGNATURE_TYPE=RSA_SHA256_2048"
            | None, Some(SignatureType.RsaSha3843072)      -> "TRANSIENT SIGNATURE_TYPE=RSA_SHA384_3072"
            | None, Some(SignatureType.RsaSha5124096)      -> "TRANSIENT SIGNATURE_TYPE=RSA_SHA512_4096"

            | None, Some(SignatureType.EdDsaSha512Ed25519) -> "TRANSIENT SIGNATURE_TYPE=EdDSA_SHA512_Ed25519"

        let createSessionString : string = 
            List.reduce (+) 
                ["SESSION CREATE STYLE=" + socketTypeToString socketType + " ";
                 "ID=" + sessionIdToString + " ";
                 "DESTINATION=" + (destinationToString(destination, signatureType))]

        System.Diagnostics.Debug.WriteLine ("Writing create session string: " + createSessionString)

        writer.WriteLine createSessionString

        System.Diagnostics.Debug.WriteLine ("Written line")

        let res = expectResponse ("SESSION", "STATUS") reader 
        match (value "RESULT" res, value "DESTINATION" res) with
        | (Some("OK"), Some(d))        -> (SessionId sessionIdToString, Destination d)
        | (Some("DUPLICATED_ID"), _)   -> raise(DuplicatedSessionIdException)
        | (Some("DUPLICATED_DEST"), _) -> raise(DuplicatedDestinationException)
        | (Some("INVALID_KEY"), _)     -> raise(InvalidKeyException)
        | _                            -> raise(ProtocolException("Unrecognized result: " + res.ToString()))
