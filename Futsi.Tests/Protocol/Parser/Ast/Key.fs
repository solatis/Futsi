module Futsi.Tests.Protocol.Parser.Ast.Key

open NUnit.Framework
open Futsi.Protocol.Parser.Ast
open FsUnit

[<Test>]
let ``Should return False on empty list``() =
    key "foo" [] |> should equal false

[<Test>]
let ``Should return True when key without value exists``() =
    key "foo" [("foo", None)] |> should equal true

[<Test>]
let ``Should return True when key with value exists``() =
    key "foo" [("foo", Some("wombat"))] |> should equal true

[<Test>]
let ``Should return False when key without value does not exist``() =
    key "bar" [("foo", None)] |> should equal false

[<Test>]
let ``Should return False when key with value exists``() =
    key "bar" [("foo", Some("wombat"))] |> should equal false
