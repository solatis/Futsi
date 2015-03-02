module Futsi.Tests.Ast

open Futsi.Ast
open Fuchu
open FsUnit

[<Tests>]
let tests =
    testList "Ast Tests" [
        testList "Looking up keys" [
            testCase "Should return False on empty list" <| fun _ ->
                key "foo" [] |> should equal false

            testCase "Should return True when key without value exists" <| fun _ ->
                key "foo" [("foo", None)] |> should equal true

            testCase "Should return True when key with value exists" <| fun _ ->
                key "foo" [("foo", Some("wombat"))] |> should equal true

            testCase "Should return False when key without value does not exist" <| fun _ ->
                key "bar" [("foo", None)] |> should equal false

            testCase "Should return False when key with value exists" <| fun _ ->
                key "bar" [("foo", Some("wombat"))] |> should equal false
            ]

        testList "Looking up values" [
            testCase "Should return None on empty list" <| fun _ ->
                value "foo" [] |> should equal None

            testCase "Should return value when key/value exists" <| fun _ ->
                value "foo" [("foo", Some("wombat"))] |> should equal (Some "wombat")

            testCase "Should return None when only key exists" <| fun _ ->
                value "foo" [("foo", None)] |> should equal None

            testCase "Should return None when key does not exist" <| fun _ ->
                value "bar" [("foo", Some("wombat"))] |> should equal None
            ]
        ]