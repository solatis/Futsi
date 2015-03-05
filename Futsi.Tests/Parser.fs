module Futsi.Tests.Parser

open Futsi.Parser
open NUnit.Framework
open FParsec.CharParsers
open Swensen.Unquote

let parseSuccess p i = 
    match run p i with
        | Success(result,_,_)   -> result
        | Failure(errorMsg,_,_) -> failwith ("Parse error occurred: " + errorMsg)

let parseFailure p i = 
    match run p i with
        | Success(result,_,_)   -> false
        | Failure(errorMsg,_,_) -> true

let testDestination : string = 
    "TedPIHKiYHLLavX~2XgghB-jYBFkwkeztWM5rwyJCO2yR2gT92FcuEahEcTrykTxafzv~4jSQOL5w0EqElqlM~PEFy5~L1pOyGB56-yVd4I-g2fsM9MGKlXNOeQinghKOcfbQx1LVY35-0X5lQSNX-8I~U7Lefukj7gSC5hieWkDS6WiUW6nYw~t061Ra0GXf2qzqFTB4nkQvnCFKaZGtNwOUUpmIbF0OtLyr6TxC7BQKgcg4jyZPS1LaBO6Wev0ZFYiQHLk4S-1LQFBfT13BxN34g-eCInwHlYeMD6NEdiy0BYHhnbBTq02HbgD3FjxW~GBBB-6a~eFABaIiJJ08XR8Mm6KKpNh~gQXut2OLxs55UhEkqk8YmTODrf6yzWzldCdaaAEVMfryO9oniWWCVl1FgLmzUHPGQ3yzvb8OlXiED2hunEfaEg0fg77FRDnYJnDHMF7i5zcUzRGb67rUa1To~H65hR9cFNWTAwX4svC-gRbbvxfi-bthyj-QqeBBQAEAAcAAOEyRS5bFHDrXnWpsjcRvpQj436gS4iCjCzdOohWgeBKC~gfLVY658op9GF6oRJ78ezPN9FBE0JqNrAM75-uL9CIeJd8JUwdldm83RNSVI1ZPZBK-5F3DgIjTsqHDMzQ9xPETiBO2UZZogXSThx9I9uYuAtg296ZhziKjYnl7wi2i3IgQlNbuPW16ajOcNeKnL1OqFipAL9e3k~LEhgBNM3J2hK1M4jO~BQ19TxIXXUfBsHFU4YjwkAOKqOxR1iP8YD~xUSfdtF9mBe6fT8-WW3-n2WgHXiTLW3PJjJuPYM4hNKNmsxsEz5vi~DE6H1pUsPVs2oXFYKZF3EcsKUVaAVWJBarBPuVNYdJgIbgl1~TJeNor8hGQw6rUTJFaZ~jjQ=="

[<TestFixture>]
type QuotedValue() = 

    [<Test>]
    member this.``Should return an empty string when the contents is empty``() =
        test <@ parseSuccess quotedValue "\"\"" = "" @>

    [<Test>]
    member this.``Should parse a normally quoted value``() = 
        test <@ parseSuccess quotedValue "\"foo\"" = "foo" @>

    [<Test>]
    member this.``Should parse a quoted value with spaces``() =
        test <@ parseSuccess quotedValue "\"foo bar\"" = "foo bar" @>

    [<Test>]
    member this.``Should parse a quoted value with an escaped quote``() = 
        test <@ parseSuccess quotedValue "\"foo \\\" bar\"" = "foo \" bar" @>

    [<Test>]
    member this.``Should parse a quoted value with an escaped newline``() = 
        test <@ parseSuccess quotedValue "\"foo \\n bar\"" = "foo \n bar" @>

    [<Test>]
    member this.``Should parse a quoted value with an escaped carriage return``() =
        test <@ parseSuccess quotedValue "\"foo \\r bar\"" = "foo \r bar" @>

    [<Test>]
    member this.``Should parse a quoted value with an escaped tab``() = 
        test <@ parseSuccess quotedValue "\"foo \\t bar\"" = "foo \t bar" @>

    [<Test>]
    member this.``Should end after a quoted value has been reached``() = 
        test <@ parseSuccess quotedValue "\"foo\" \"bar\"" = "foo" @>


[<TestFixture>]
type UnquotedValue() = 

    [<Test>]
    member this.``Should return the correct value when a value is provided``() = 
        test <@ parseSuccess unquotedValue "foo" = "foo" @>

    [<Test>]
    member this.``Should stop after whitespace``() = 
        test <@ parseSuccess unquotedValue "foo bar" = "foo"  @>
        test <@ parseSuccess unquotedValue "foo\tbar" = "foo" @>

    [<Test>]
    member this.``Should stop after a newline``() = 
        test <@ parseSuccess unquotedValue "foo\nbar" = "foo" @>
        test <@ parseSuccess unquotedValue "foo\r\nbar" = "foo" @>

    [<Test>]
    member this.``Should parse a destination``() = 
        test <@ parseSuccess unquotedValue testDestination = testDestination @>

    [<Test>]
    member this.``Should parse a destination and stop after the destination``() = 
        test <@ parseSuccess unquotedValue (testDestination + " foobar") = testDestination @>

[<TestFixture>]
type Key() = 

    [<Test>]
    member this.``Should succeed when providing a simple key``() = 
        test <@ parseSuccess key "foo" = "foo" @>

    [<Test>]
    member this.``Should succeed when providing a space separated key``() = 
        test <@ parseSuccess key "foo key" = "foo" @>

    [<Test>]
    member this.``Should succeed when providing an equals separated key``() = 
        test <@ parseSuccess key "foo=key" = "foo" @>    

    [<Test>]
    member this.``Should stop after newline``() = 
        test <@ parseSuccess key "foo\nbar" = "foo" @>    


    [<Test>]
    member this.``Should fail on empty input``() = 
        test <@ parseFailure key "" = true @>

[<TestFixture>]
type Parameter() = 

    [<Test>]
    member this.``Should parse a key when only providing a key``() = 
        test <@ parseSuccess parameter "foo" = ("foo", None) @>

    [<Test>]
    member this.``Should parse a key/value when only providing a key/value``() = 
        test <@ parseSuccess parameter "foo=bar" = ("foo", Some "bar") @>

[<TestFixture>]
type Tokens() = 

    [<Test>]
    member this.``Should parse a single key token``() = 
        test <@ parseSuccess tokens "foo" = [("foo", None)] @>

    [<Test>]
    member this.``Should parse a single key/value token``() = 
        test <@ parseSuccess tokens "foo=bar" = [("foo", Some "bar")] @>

    [<Test>]
    member this.``Should parse a multiple key tokens``() = 
        test <@ parseSuccess tokens "foo wom" = [("foo", None); ("wom", None)] @>

    [<Test>]
    member this.``Should parse a multiple key/value tokens``() = 
        test <@ parseSuccess tokens "foo=bar wom=bat" = [("foo", Some "bar"); ("wom", Some "bat")] @>

    [<Test>]
    member this.``Should parse a keys and key/value tokens combined``() = 
        test <@ parseSuccess tokens "foo=bar baz wom=bat" = [("foo", Some "bar"); ("baz", None); ("wom", Some "bat")] @>

[<TestFixture>]
type Line() = 

    [<Test>]
    member this.``Should parse multiple tokens``() = 
        test <@ parseSuccess line "foo=bar baz" = [("foo", Some "bar"); ("baz", None)] @>

    [<Test>]
    member this.``Should fail when newline is provided``() = 
        test <@ parseFailure line "foo=bar baz\n" = true @>
