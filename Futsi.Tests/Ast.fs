module Futsi.Tests.Ast

open Futsi.Ast
open FsUnit
open NUnit.Framework

[<TestFixture>]
type Key() = 

    [<Test>]
    member this.``Should return False on empty list``() = 
        key "foo" [] |> should equal false

    [<Test>]
    member this.``Should return True when key without value exists``() = 
        key "foo" [("foo", None)] |> should equal true

    [<Test>]
    member this.``Should return True when key with value exists``() = 
        key "foo" [("foo", Some("wombat"))] |> should equal true

    [<Test>]
    member this.``Should return False when key without value does not exist``() = 
        key "bar" [("foo", None)] |> should equal false

    member this.``Should return False when key with value exists``() = 
        key "bar" [("foo", Some("wombat"))] |> should equal false

[<TestFixture>]
type Value() = 

    [<Test>]
    member this.``Should return None on empty list``() = 
        value "foo" [] |> should equal None

    [<Test>]
    member this.``Should return value when key/value exists``() =
        value "foo" [("foo", Some("wombat"))] |> should equal (Some "wombat")

    [<Test>]
    member this.``Should return None when only key exists``() = 
        value "foo" [("foo", None)] |> should equal None

    [<Test>]
    member this.``Should return None when key does not exist``() = 
        value "bar" [("foo", Some("wombat"))] |> should equal None
