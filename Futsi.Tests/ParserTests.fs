module Futsi.Tests.Parser

open Futsi.Parser
open FsUnit
open NUnit.Framework
open FParsec.CharParsers

let parseSuccess p i = 
    match run p i with
        | Success(result,_,_)   -> result
        | Failure(errorMsg,_,_) -> failwith "Parse error occurred: %s" errorMsg

let testDestination : string = 
    "TedPIHKiYHLLavX~2XgghB-jYBFkwkeztWM5rwyJCO2yR2gT92FcuEahEcTrykTxafzv~4jSQOL5w0EqElqlM~PEFy5~L1pOyGB56-yVd4I-g2fsM9MGKlXNOeQinghKOcfbQx1LVY35-0X5lQSNX-8I~U7Lefukj7gSC5hieWkDS6WiUW6nYw~t061Ra0GXf2qzqFTB4nkQvnCFKaZGtNwOUUpmIbF0OtLyr6TxC7BQKgcg4jyZPS1LaBO6Wev0ZFYiQHLk4S-1LQFBfT13BxN34g-eCInwHlYeMD6NEdiy0BYHhnbBTq02HbgD3FjxW~GBBB-6a~eFABaIiJJ08XR8Mm6KKpNh~gQXut2OLxs55UhEkqk8YmTODrf6yzWzldCdaaAEVMfryO9oniWWCVl1FgLmzUHPGQ3yzvb8OlXiED2hunEfaEg0fg77FRDnYJnDHMF7i5zcUzRGb67rUa1To~H65hR9cFNWTAwX4svC-gRbbvxfi-bthyj-QqeBBQAEAAcAAOEyRS5bFHDrXnWpsjcRvpQj436gS4iCjCzdOohWgeBKC~gfLVY658op9GF6oRJ78ezPN9FBE0JqNrAM75-uL9CIeJd8JUwdldm83RNSVI1ZPZBK-5F3DgIjTsqHDMzQ9xPETiBO2UZZogXSThx9I9uYuAtg296ZhziKjYnl7wi2i3IgQlNbuPW16ajOcNeKnL1OqFipAL9e3k~LEhgBNM3J2hK1M4jO~BQ19TxIXXUfBsHFU4YjwkAOKqOxR1iP8YD~xUSfdtF9mBe6fT8-WW3-n2WgHXiTLW3PJjJuPYM4hNKNmsxsEz5vi~DE6H1pUsPVs2oXFYKZF3EcsKUVaAVWJBarBPuVNYdJgIbgl1~TJeNor8hGQw6rUTJFaZ~jjQ=="

[<TestFixture>]
type QuotedValue() = 

    [<Test>]
    member this.``Should return an empty string when the contents is empty``() =
        parseSuccess quotedValue "\"\"" |> should equal ""

    [<Test>]
    member this.``Should parse a normally quoted value``() = 
        parseSuccess quotedValue "\"foo\"" |> should equal "foo"

    [<Test>]
    member this.``Should parse a quoted value with spaces``() =
        parseSuccess quotedValue "\"foo bar\"" |> should equal "foo bar"

    [<Test>]
    member this.``Should parse a quoted value with an escaped quote``() = 
        parseSuccess quotedValue "\"foo \\\" bar\"" |> should equal "foo \" bar"

    [<Test>]
    member this.``Should parse a quoted value with an escaped newline``() = 
        parseSuccess quotedValue "\"foo \\n bar\"" |> should equal "foo \n bar"

    [<Test>]
    member this.``Should parse a quoted value with an escaped carriage return``() =
        parseSuccess quotedValue "\"foo \\r bar\"" |> should equal "foo \r bar"

    [<Test>]
    member this.``Should parse a quoted value with an escaped tab``() = 
        parseSuccess quotedValue "\"foo \\t bar\"" |> should equal "foo \t bar"

    [<Test>]
    member this.``Should end after a quoted value has been reached``() = 
        parseSuccess quotedValue "\"foo\" \"bar\"" |> should equal "foo"


[<TestFixture>]
type UnquotedValue() = 

    [<Test>]
    member this.``Should return the correct value when a value is provided``() = 
        parseSuccess unquotedValue "foo" |> should equal "foo"

    [<Test>]
    member this.``Should stop after whitespace``() = 
        parseSuccess unquotedValue "foo bar" |> should equal "foo"  
        parseSuccess unquotedValue "foo\tbar" |> should equal "foo"

    [<Test>]
    member this.``Should stop after a newline``() = 
        parseSuccess unquotedValue "foo\nbar" |> should equal "foo"
        parseSuccess unquotedValue "foo\r\nbar" |> should equal "foo"

    [<Test>]
    member this.``Should parse a destination``() = 
        parseSuccess unquotedValue testDestination |> should equal testDestination

    [<Test>]
    member this.``Should parse a destination and stop after the destination``() = 
        parseSuccess unquotedValue (testDestination + " foobar") |> should equal testDestination

[<TestFixture>]
type Key() = 

    [<Test>]
    member this.``Should succeed when providing a simple key``() = 
        parseSuccess key "foo" |> should equal ("foo", Option<string>.None)
