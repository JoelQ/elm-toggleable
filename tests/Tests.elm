module Tests exposing (closed, open, suite, toggleable)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Test exposing (..)
import Toggleable exposing (Toggleable(..))


open : Fuzzer (Toggleable String)
open =
    Fuzz.string
        |> Fuzz.map Open


closed : Fuzzer (Toggleable String)
closed =
    Fuzz.string
        |> Fuzz.map Closed


toggleable : Fuzzer (Toggleable String)
toggleable =
    Fuzz.oneOf [ closed, open ]


suite : Test
suite =
    describe "The Toggleable module"
        [ describe "Toggleable.toggle"
            [ fuzz toggleable "Toggling once changes the value" <|
                \before ->
                    Toggleable.toggle before
                        |> Expect.notEqual before
            , fuzz toggleable "Toggling twice is the same value" <|
                \before ->
                    Toggleable.toggle (Toggleable.toggle before)
                        |> Expect.equal before
            , fuzz toggleable "Toggling does not change tagged value" <|
                \before ->
                    Toggleable.unwrap (Toggleable.toggle before)
                        |> Expect.equal (Toggleable.unwrap before)
            ]
        , describe "Toggleable.toggleIf"
            [ fuzz toggleable "Toggling once changes the value" <|
                \before ->
                    Toggleable.toggleIf (always True) before
                        |> Expect.notEqual before
            , fuzz toggleable "Toggling twice is the same value" <|
                \before ->
                    before
                        |> Toggleable.toggleIf (always True)
                        |> Toggleable.toggleIf (always True)
                        |> Expect.equal before
            , fuzz toggleable "Toggling does not change tagged value" <|
                \before ->
                    Toggleable.unwrap (Toggleable.toggleIf (always True) before)
                        |> Expect.equal (Toggleable.unwrap before)
            , fuzz toggleable "when the expression is false does not change" <|
                \before ->
                    before
                        |> Toggleable.toggleIf (always False)
                        |> Expect.equal before
            ]
        , describe "Toggleable.open"
            [ fuzz toggleable "Opening always results in an open toggle" <|
                \before ->
                    Toggleable.open before
                        |> Expect.equal (Open <| Toggleable.unwrap before)
            , fuzz toggleable "Opening does not change tagged value" <|
                \before ->
                    Toggleable.unwrap (Toggleable.open before)
                        |> Expect.equal (Toggleable.unwrap before)
            ]
        , describe "Toggleable.close"
            [ fuzz toggleable "Closing always results in an closed toggle" <|
                \before ->
                    Toggleable.close before
                        |> Expect.equal (Closed <| Toggleable.unwrap before)
            , fuzz toggleable "Closing does not change tagged value" <|
                \before ->
                    Toggleable.unwrap (Toggleable.close before)
                        |> Expect.equal (Toggleable.unwrap before)
            ]
        , describe "Toggleable.map"
            [ fuzz toggleable "Functor identity law" <|
                \before ->
                    Toggleable.map identity before
                        |> Expect.equal before
            , fuzz toggleable "Functor composition law" <|
                \before ->
                    before
                        |> Toggleable.map String.reverse
                        |> Toggleable.map String.toUpper
                        |> Expect.equal (Toggleable.map (String.toUpper << String.reverse) before)
            ]
        ]
