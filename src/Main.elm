module Main exposing (Msg(..), main, update, view)

import Browser
import Html exposing (Html, button, div, span, text)
import Html.Events exposing (onClick)


type World
    = World (List (List Int))


type alias Model =
    { world : World
    }


init : Model
init =
    Model (World [ [ 1, 2, 3 ], [ 4, 5, 6 ], [ 7, 8, 9 ] ])


main =
    Browser.sandbox { init = init, update = update, view = view }


type Msg
    = NoOp


update : Msg -> Model -> Model
update msg model =
    case msg of
        NoOp ->
            model


view : Model -> Html Msg
view model =
    div []
        [ viewWorld model.world ]


viewRow : List Int -> Html Msg
viewRow row =
    div [] (List.map viewCell row)


viewWorld : World -> Html Msg
viewWorld (World entries) =
    div [] (List.map viewRow entries)


viewCell : Int -> Html Msg
viewCell v =
    span [] [ text (String.fromInt v) ]
