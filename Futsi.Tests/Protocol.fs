module Futsi.Tests.Protocol

open Futsi.Protocol
open NUnit.Framework
open Swensen.Unquote

[<TestFixture>]
type Connect() = 

    [<Test>]
    member this.``Can connect with SAM bridge``() =
        let callback (stream: System.IO.StreamReader) (writer: System.IO.StreamWriter) = "success"
        test <@ connect "127.0.0.1" 7656 callback = "success" @>

    [<Test>]
    member this.``Fails on a non-existent port``() =
        let callback (stream: System.IO.StreamReader) (client: System.IO.StreamWriter) = "success"
        raises<System.Net.Sockets.SocketException> <@ connect "127.0.0.1" 1234 callback = "success" @>

[<TestFixture>]
type Version() = 

    [<Test>]
    member this.``Should negotiate protocol version 3,1 when we do not use a constraint``() =
        test <@ connect "127.0.0.1" 7656 version = [3;1] @>

    [<Test>]
    member this.``Should negotiate protocol version 3,0 when we put a constraint in place``() =
        test <@ connect "127.0.0.1" 7656 (versionWithConstraint([3;0],[3;0])) = [3;0] @>
