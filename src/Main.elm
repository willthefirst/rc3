module Main exposing (Msg(..), main, update, view)

import Array exposing (Array)
import Browser
import Html exposing (Html, button, div, span, text)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick)


type Color
    = White
    | Blue
    | Black


nextColor : Color -> Color
nextColor input =
    case input of
        White ->
            Blue

        Blue ->
            Black

        Black ->
            White


type World
    = World (Array (Array Color))


type alias Model =
    { world : World
    }


init : Model
init =
    Model
        (World
            (Array.fromList
                [ Array.fromList [ White, Blue, Black ]
                , Array.fromList [ Blue, Black, Blue ]
                , Array.fromList [ Blue, White, Blue ]
                ]
            )
        )


main =
    Browser.sandbox { init = init, update = update, view = view }


type Msg
    = ChangeColor Int Int -- X and Y coordinates in the grid.


update : Msg -> Model -> Model
update msg model =
    let
        (World world) =
            model.world
    in
    case msg of
        ChangeColor xCoordinate yCoordinate ->
            case Array.get yCoordinate world of
                Nothing ->
                    model

                Just row ->
                    case Array.get xCoordinate row of
                        Nothing ->
                            model

                        Just color ->
                            { model
                                | world =
                                    World (Array.set yCoordinate (Array.set xCoordinate (nextColor color) row) world)
                            }


view : Model -> Html Msg
view model =
    div []
        [ viewWorld model.world ]


viewRow : Int -> Array Color -> Html Msg
viewRow yCoordinate row =
    div [] (Array.toList (Array.indexedMap (viewCell yCoordinate) row))


viewWorld : World -> Html Msg
viewWorld (World entries) =
    div [] (Array.toList (Array.indexedMap viewRow entries))


viewCell : Int -> Int -> Color -> Html Msg
viewCell yCoordinate xCoordinate color =
    let
        cssColor =
            case color of
                White ->
                    "white"

                Blue ->
                    "lightblue"

                Black ->
                    "black"
    in
    span
        [ style "display" "inline-block"
        , style "background-color" cssColor
        , style "width" cellSize
        , style "border" "solid 1px lightgray"
        , style "height" "3em"
        , onClick (ChangeColor xCoordinate yCoordinate)
        ]
        []


cellSize : String
cellSize =
    "2em"
