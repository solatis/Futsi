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
    exception InvalidIdException
    exception TimeoutException
    exception UnreachableException

    /// <summary>
    ///   This function parses the two first commands/keywords.
    /// </summary>
    /// <remarks>
    ///   The first two commands of a response should always be in a specific
    ///   order, and the rest of the response contains the actual data we are
    ///   looking for.
    /// </remarks>
    /// <returns>Remaining tokens</returns>
    let expectResponse (k1 : string, k2 : string) (reader: StreamReader) : Token list = 
        let tokens = runStream Parser.line reader
        match tokens with
        | (k1, None) :: (k2, None) :: xs -> xs
        | _                              -> raise (ProtocolException ("Invalid response: " + tokens.ToString()))

    /// Connects to host and executes within a callback. Closes connection
    /// once execution completes.
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

    /// <summary>Negotiates a specific protocol version with the SAM host bridge</summary>
    /// <returns>Integer representation of negotiated version as list. [3;1] means version 3.1</returns>
    let versionWithConstraint (min,max) (reader: StreamReader) (writer : StreamWriter) : int list =
        /// Converts an int array [3;1] to the string "3.1"
        let versionToString version =
            String.concat "." (List.map string version)

        /// Generates the string to announce ourselves with the SAM bridge
        let helloString : string = 
            List.reduce (+) 
                ["HELLO VERSION ";
                 "MIN=" + versionToString min + " ";
                 "MAX=" + versionToString max]

        /// Converts a string "3.1" to an int array [3;1]
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

    /// Default implementation of version negotiation which defaults to version 3.1
    let version : (StreamReader -> StreamWriter -> int list) = 
        versionWithConstraint ([3;1],[3;1])

    /// <summary>Creates a new session</summary>
    /// <remarks>
    ///   Allows the user to optionally provide a specific session id and destination. 
    ///
    ///   This session id can then be used in a separate connection to the SAM bridge to either 
    ///   accept a new connection, or connect to a remote destination. 
    ///
    ///   As soon as the 'master' connection with the SAM bridge is lost, the session and its 
    ///   associated destination will be cleaned up.
    /// </remarks>
    /// <returns>Session id and destination created.</returns>
    let createSessionWith sessionId destination signatureType socketType (reader: StreamReader) (writer : StreamWriter) =

        /// Converts socket type to a string representation as used within the SAMv3 protocol.
        let socketTypeToString = function
            | SocketType.VirtualStream     -> "STREAM"
            | SocketType.DatagramRepliable -> "DATAGRAM"
            | SocketType.DatagramAnonymous -> "RAW"

        /// <summary>Converts a Destination to a string as used in the protocol.</summary>
        /// <remarks>
        ///   When no destination is provided, a user can optionally provide a signature type, 
        ///   in which case that encryption algorithm will be used.
        /// </remarks>
        /// <returns>SAMv3 string protocol for creating destination</returns>
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

        /// Variable that holds the session id in a string representation. If no SessionId
        /// was provided, generates a new one based on a GUID
        let sessionIdAsString : string = 
            match sessionId with
            | None                -> System.Guid.NewGuid().ToString()
            | Some(SessionId(id)) -> id

        /// The SAMv3 protocol representation on how to create a session.
        let createSessionString : string = 
            List.reduce (+) 
                ["SESSION CREATE STYLE=" + socketTypeToString socketType + " ";
                 "ID=" + sessionIdAsString + " ";
                 "DESTINATION=" + (destinationToString(destination, signatureType))]

        System.Diagnostics.Debug.WriteLine ("Writing create session string: " + createSessionString)

        writer.WriteLine createSessionString

        System.Diagnostics.Debug.WriteLine ("Written line")

        let res = expectResponse ("SESSION", "STATUS") reader 
        match (value "RESULT" res, value "DESTINATION" res) with
        | (Some("OK"), Some(d))        -> (SessionId sessionIdAsString, Destination d)
        | (Some("DUPLICATED_ID"), _)   -> raise(DuplicatedSessionIdException)
        | (Some("DUPLICATED_DEST"), _) -> raise(DuplicatedDestinationException)
        | (Some("INVALID_KEY"), _)     -> raise(InvalidKeyException)
        | _                            -> raise(ProtocolException("Unrecognized result: " + res.ToString()))

    /// <summary>Establishes connection with remote destination using VirtualStream.</summary>
    let connectStream (SessionId sessionId) (Destination destination) (reader: StreamReader) (writer: StreamWriter) = 
        let connectStreamString : string = 
            List.reduce (+) 
                [ "STREAM CONNECT ";
                  "ID=" + sessionId + " ";
                  "DESTINATION=" + destination + " ";
                  "SILENT=false"]

        System.Diagnostics.Debug.WriteLine ("Writing connect stream string: " + connectStreamString)

        writer.WriteLine connectStreamString

        System.Diagnostics.Debug.WriteLine ("Written line")

        let res = expectResponse ("STREAM", "STATUS") reader 
        match value "RESULT" res with
        | Some("OK")              -> ()
        | Some("INVALID_ID")      -> raise(InvalidIdException)
        | Some("INVALID_KEY")     -> raise(InvalidKeyException)
        | Some("TIMEOUT")         -> raise(TimeoutException)
        | Some("CANT_REACH_PEER") -> raise(UnreachableException)
        | _                       -> raise(ProtocolException("Unrecognized result: " + res.ToString()))
                
