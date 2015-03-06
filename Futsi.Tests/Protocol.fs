module Futsi.Tests.Protocol

open NUnit.Framework
open Swensen.Unquote

open Futsi.Protocol
open Futsi.Types

[<TestFixture>]
type Connect() = 

    [<Test>]
    member this.``Can connect with SAM bridge``() =
        let callback (stream: System.IO.StreamReader) (writer: System.IO.StreamWriter) = "success"
        test <@ connect (HostName "127.0.0.1") (Port 7656) callback = "success" @>

    [<Test>]
    member this.``Fails on a non-existent port``() =
        let callback (stream: System.IO.StreamReader) (client: System.IO.StreamWriter) = "success"
        raises<System.Net.Sockets.SocketException> <@ connect (HostName "127.0.0.1") (Port 1234) callback = "success" @>

[<TestFixture>]
type Version() = 

    [<Test>]
    member this.``Should negotiate protocol version 3,1 when we do not use a constraint``() =
        test <@ connect (HostName "127.0.0.1") (Port 7656) version = [3;1] @>

    [<Test>]
    member this.``Should negotiate protocol version 3,0 when we put a constraint in place``() =
        test <@ connect (HostName "127.0.0.1") (Port 7656) (versionWithConstraint([3;0],[3;0])) = [3;0] @>

    [<Test>]
    member this.``Should throw an exception if we request a non-existant protocol``() =
        raises<NoVersionException> <@ connect (HostName "127.0.0.1") (Port 7656) (versionWithConstraint([9;0],[9;0])) = [3;0] @>

[<TestFixture>]
type CreateSession() = 

    [<Test>]
    member this.``Should be able to create sessions with all socket types``() =
        let phase1 socketType reader writer =             
            version reader writer |> ignore
            createSessionWith None None None socketType reader writer

        let performTest (socketType : SocketType) = 
            connect (HostName "127.0.0.1") (Port 7656) (phase1 socketType) |> ignore

        test <@ List.map performTest [SocketType.VirtualStream; 
                                      SocketType.DatagramAnonymous; 
                                      SocketType.DatagramRepliable] |> ignore = () @>

    [<Test>]
    member this.``Should be able to create sessions with all signature types``() =
        let phase1 signatureType reader writer =             
            version reader writer |> ignore
            createSessionWith None None (Some signatureType) SocketType.VirtualStream reader writer

        let performTest (signatureType : SignatureType) = 
            connect (HostName "127.0.0.1") (Port 7656) (phase1 signatureType) |> ignore

        test <@ List.map performTest [SignatureType.DsaSha1;
                                      SignatureType.EcdsaSha256P256;
                                      SignatureType.EcdsaSha384P384;
                                      SignatureType.EcdsaSha512P521;
                                      SignatureType.RsaSha2562048;
                                      SignatureType.RsaSha3843072;
                                      SignatureType.RsaSha5124096;
                                      SignatureType.EdDsaSha512Ed25519] |> ignore = () @>

    [<Test>]
    member this.``Should throw a protocol error when creating a session twice``() =
        let performTest reader writer = 
            version reader writer |> ignore
            createSessionWith None None None SocketType.VirtualStream reader writer |> ignore
            createSessionWith None None None SocketType.VirtualStream reader writer |> ignore

        raises<ProtocolException> <@ connect (HostName "127.0.0.1") (Port 7656) performTest @>

    [<Test>]
    member this.``Should throw an error when using the same session id in two sessions``() =
        let phase2 sessionId reader writer = 
            version reader writer |> ignore
            createSessionWith (Some sessionId) None None SocketType.VirtualStream reader writer

        let phase1 reader writer : unit = 
            version reader writer |> ignore
            let (sessionId, _) = createSessionWith None None None SocketType.VirtualStream reader writer
            connect (HostName "127.0.0.1") (Port 7656)  (phase2 sessionId) |> ignore

        raises<DuplicatedSessionIdException> <@ connect (HostName "127.0.0.1") (Port 7656) phase1 @>

    [<Test>]
    member this.``Should throw an error when reusing the same destination``() =
        let phase2 destination reader writer = 
            version reader writer |> ignore
            createSessionWith None (Some destination) None SocketType.VirtualStream reader writer

        let phase1 reader writer : unit = 
            version reader writer |> ignore
            let (_, destination) = createSessionWith None None None SocketType.VirtualStream reader writer
            connect (HostName "127.0.0.1") (Port 7656)  (phase2 destination) |> ignore

        raises<DuplicatedDestinationException> <@ connect (HostName "127.0.0.1") (Port 7656) phase1 @>

    [<Test>]
    member this.``Should throw an error when providing an invalid destination``() =
        let phase1 reader writer : unit = 
            version reader writer |> ignore
            createSessionWith None (Some (Destination "invalid123")) None SocketType.VirtualStream reader writer |> ignore

        raises<InvalidKeyException> <@ connect (HostName "127.0.0.1") (Port 7656) phase1 @>
