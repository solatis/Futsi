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

        test <@ List.map performTest [SocketType.VirtualStream; SocketType.DatagramAnonymous; SocketType.DatagramRepliable] |> ignore = () @>
