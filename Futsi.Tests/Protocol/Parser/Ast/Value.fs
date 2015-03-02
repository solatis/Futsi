module Futsi.Tests.Protocol.Parser.Ast.Value

open NUnit.Framework
open Futsi.Protocol.Parser.Ast
open FsUnit

[<Test>]
let ``Should return None on empty list``() =
    value "foo" [] |> should equal None

[<Test>]
let ``Should return value when key/value exists``() =
    value "foo" [("foo", Some("wombat"))] |> should equal (Some "wombat")

[<Test>]
let ``Should return None when only key exists``() =
    value "foo" [("foo", None)] |> should equal None

[<Test>]
let ``Should return None when key does not exist``() =
    value "bar" [("foo", Some("wombat"))] |> should equal None