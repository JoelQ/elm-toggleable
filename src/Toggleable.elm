module Toggleable exposing
    ( Toggleable(..)
    , toggle, toggleIf, open, close
    , unwrap
    , map
    )

{-| A representation of values in a toggleable UI as well as functions for
common transformations. The `Toggleable` type models values that are
independently toggleable. See it in action in this
[live example](https://ellie-app.com/kPBk4HDKQa1/1).

If you are looking to implement "accordion"-style behavior, you may want to
use a zipper implementation instead such as
[SelectList](http://package.elm-lang.org/packages/rtfeldman/selectlist/latest).


# Definition

@docs Toggleable


# Toggling

@docs toggle, toggleIf, open, close


# Unwrapping

@docs unwrap


# Transforming

@docs map

-}


{-| Represent a value that might be toggled open or closed. You can put these
directly in your model.

    { article = "a lot of text..."
    , comments = [ Open comment1, Closed comment2 ]
    }

-}
type Toggleable a
    = Open a
    | Closed a


{-| Toggle from `Open` to `Closed` or vice-versa.

    toggle (Open "hello") -- Closed "hello"

    toggle (Closed "hello") -- Open "hello"

-}
toggle : Toggleable a -> Toggleable a
toggle toggleable =
    case toggleable of
        Open value ->
            Closed value

        Closed value ->
            Open value


{-| Only toggle if the given expression is True. The value inside the toggleable
remains unchanged.

    toggleIf String.isEmpty (Open "") -- Closed ""

    toggleIf String.isEmpty (Open "hello") -- Open "hello"

-}
toggleIf : (a -> Bool) -> Toggleable a -> Toggleable a
toggleIf function toggleable =
    if function <| unwrap toggleable then
        toggle toggleable

    else
        toggleable


{-| Convert a any toggleable value to `Open`.

    open (Closed "hello") -- Open "hello"

    open (Open "hello") -- Open "hello

-}
open : Toggleable a -> Toggleable a
open toggleable =
    case toggleable of
        Open value ->
            Open value

        Closed value ->
            Open value


{-| Convert a any toggleable value to `Closed`.

    close (Closed "hello") -- Closed "hello"

    close (Open "hello") -- Closed "hello

-}
close : Toggleable a -> Toggleable a
close toggleable =
    case toggleable of
        Open value ->
            Closed value

        Closed value ->
            Closed value


{-| Get the value inside the toggleable.

    unwrap (Open "hello") -- "hello"

-}
unwrap : Toggleable a -> a
unwrap toggleable =
    case toggleable of
        Open value ->
            value

        Closed value ->
            value


{-| Transform the value inside the toggleable _without_ modifying the toggleable
state.

    map String.toUpper (Open "hello") -- Open "HELLO"

    map String.toUpper (Closed "hello") -- Closed "HELLO"

-}
map : (a -> b) -> Toggleable a -> Toggleable b
map function toggleable =
    case toggleable of
        Open value ->
            Open (function value)

        Closed value ->
            Closed (function value)
