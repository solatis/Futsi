module Futsi.Tests.Protocol

open NUnit.Framework
open Swensen.Unquote

open Futsi.Protocol
open Futsi.Types

let testDestination : string = 
    "TedPIHKiYHLLavX~2XgghB-jYBFkwkeztWM5rwyJCO2yR2gT92FcuEahEcTrykTxafzv~4jSQOL5w0EqElqlM~PEFy5~L1pOyGB56-yVd4I-g2fsM9MGKlXNOeQinghKOcfbQx1LVY35-0X5lQSNX-8I~U7Lefukj7gSC5hieWkDS6WiUW6nYw~t061Ra0GXf2qzqFTB4nkQvnCFKaZGtNwOUUpmIbF0OtLyr6TxC7BQKgcg4jyZPS1LaBO6Wev0ZFYiQHLk4S-1LQFBfT13BxN34g-eCInwHlYeMD6NEdiy0BYHhnbBTq02HbgD3FjxW~GBBB-6a~eFABaIiJJ08XR8Mm6KKpNh~gQXut2OLxs55UhEkqk8YmTODrf6yzWzldCdaaAEVMfryO9oniWWCVl1FgLmzUHPGQ3yzvb8OlXiED2hunEfaEg0fg77FRDnYJnDHMF7i5zcUzRGb67rUa1To~H65hR9cFNWTAwX4svC-gRbbvxfi-bthyj-QqeBBQAEAAcAAOEyRS5bFHDrXnWpsjcRvpQj436gS4iCjCzdOohWgeBKC~gfLVY658op9GF6oRJ78ezPN9FBE0JqNrAM75-uL9CIeJd8JUwdldm83RNSVI1ZPZBK-5F3DgIjTsqHDMzQ9xPETiBO2UZZogXSThx9I9uYuAtg296ZhziKjYnl7wi2i3IgQlNbuPW16ajOcNeKnL1OqFipAL9e3k~LEhgBNM3J2hK1M4jO~BQ19TxIXXUfBsHFU4YjwkAOKqOxR1iP8YD~xUSfdtF9mBe6fT8-WW3-n2WgHXiTLW3PJjJuPYM4hNKNmsxsEz5vi~DE6H1pUsPVs2oXFYKZF3EcsKUVaAVWJBarBPuVNYdJgIbgl1~TJeNor8hGQw6rUTJFaZ~jjQ=="

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

[<TestFixture>]
type ConnectStream() = 

    [<Test>]
    member this.``Should throw an error when providing an invalid session``() =
        let phase1 reader writer : unit = 
            version reader writer |> ignore
            connectStream (SessionId "invalidSessionId") (Destination "notRelevant") reader writer |> ignore

        raises<InvalidIdException> <@ connect (HostName "127.0.0.1") (Port 7656) phase1 @>

    [<Test>]
    member this.``Should throw an error when not using a VirtualStream``() =
        
        let phase2 sessionId reader writer = 
            version reader writer |> ignore
            connectStream sessionId (Destination "notRelevant") reader writer |> ignore

        let phase1 socketType reader writer = 
            version reader writer |> ignore
            let (sessionId, _) = createSessionWith None None None socketType reader writer

            connect (HostName "127.0.0.1") (Port 7656) (phase2 sessionId) |> ignore

        let performTest socketType =
            connect (HostName "127.0.0.1") (Port 7656) (phase1 socketType) |> ignore

        raises<ProtocolException> <@ performTest SocketType.DatagramAnonymous @>
        raises<ProtocolException> <@ performTest SocketType.DatagramRepliable @>


    [<Test>]
    member this.``Should throw an appropriate error if the remote cannot be reached``() =
        
        let phase2 sessionId reader writer = 
            version reader writer |> ignore
            connectStream sessionId (Destination testDestination) reader writer |> ignore

        let phase1 reader writer = 
            version reader writer |> ignore
            let (sessionId, _) = createSessionWith None None None SocketType.VirtualStream reader writer

            connect (HostName "127.0.0.1") (Port 7656) (phase2 sessionId) |> ignore

        raises<UnreachableException> <@ connect (HostName "127.0.0.1") (Port 7656) phase1 |> ignore @>

