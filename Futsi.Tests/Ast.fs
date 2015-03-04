module Futsi.Tests.Ast

open Futsi.Ast
open NUnit.Framework
open Swensen.Unquote

[<TestFixture>]
type Key() = 

    [<Test>]
    member this.``Should return False on empty list``() = 
        test <@ key "foo" [] = false @>

    [<Test>]
    member this.``Should return True when key without value exists``() = 
        test <@ key "foo" [("foo", None)] = true @>

    [<Test>]
    member this.``Should return True when key with value exists``() = 
        test <@ key "foo" [("foo", Some("wombat"))] = true @>

    [<Test>]
    member this.``Should return False when key without value does not exist``() = 
        test <@ key "bar" [("foo", None)] = false @>

    member this.``Should return False when key with value exists``() = 
        test <@ key "bar" [("foo", Some("wombat"))] = false @>

[<TestFixture>]
type Value() = 

    [<Test>]
    member this.``Should return None on empty list``() = 
        test <@ value "foo" [] = None @>

    [<Test>]
    member this.``Should return value when key/value exists``() =
        test <@ value "foo" [("foo", Some("wombat"))] = Some "wombat" @>

    [<Test>]
    member this.``Should return None when only key exists``() = 
        test <@ value "foo" [("foo", None)] = None @>

    [<Test>]
    member this.``Should return None when key does not exist``() = 
        test <@ value "bar" [("foo", Some("wombat"))] = None @>
