module Main exposing (Msg(..), main, update, view)

import Array exposing (Array)
import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onDoubleClick)
import Random


type alias User =
    { profile : String
    , x : Int
    , y : Int
    }



--
-- Note:
-- Add your photo here:
--


users : List User
users =
    [ User "https://kofi.sexy/images/profile.jpg" 0 0
    , User "https://raw.githubusercontent.com/captn3m0/avatars/master/gravatar/cuber.jpg" 1 2
    , User "https://d29xw0ra2h4o4u.cloudfront.net/assets/people/justin_holzmann_150-99cc8a73d0752979e3e6900e31c6e4a051fc2e5856920a87b58f26e30db8aa7b.jpg" 2 2
    , User "https://d29xw0ra2h4o4u.cloudfront.net/assets/people/raunak_singh_150-121093425a4819b4f050b1076621a5288c310b29787c0e5ff7774d95a02363b7.jpg" 2 1
    , User "https://s.gravatar.com/avatar/b2d41e7def3ee97a7bd5612e7fe2f604?s=80" 1 3
    ]


type Color
    = White
    | Black
    | Violet
    | Indigo
    | Blue
    | Green
    | Yellow
    | Orange
    | Red


nextColor : Color -> Color
nextColor input =
    case input of
        White ->
            Black

        Black ->
            Violet

        Violet ->
            Indigo

        Indigo ->
            Blue

        Blue ->
            Green

        Green ->
            Yellow

        Yellow ->
            Orange

        Orange ->
            Red

        Red ->
            White


randomIshColor : Random.Generator Color
randomIshColor =
    Random.weighted ( 80, White )
        [ ( 10, Black )
        , ( 10, Violet )
        , ( 10, Indigo )
        , ( 10, Blue )
        , ( 10, Green )
        , ( 10, Yellow )
        , ( 10, Orange )
        , ( 10, Red )
        ]


type World
    = World (Array (Array Color))


type alias Model =
    { world : World
    , users : List User
    , selectedUser : Maybe User
    }


type alias Flags =
    Int


init : Flags -> ( Model, Cmd Msg )
init flags =
    ( Model
        (World
            (Array.repeat 20 (Array.repeat 20 White))
        )
        users
        Nothing
    , SetSomeRandom
    )


main =
    Browser.document
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }


type Msg
    = ChangeColor Int Int -- X and Y coordinates in the grid.
    | GoToRoom Int Int -- X and Y coordinates in the grid.
    | Select User
    | SetSomeRandom


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        (World world) =
            model.world
    in
    case msg of
        GoToRoom xCoordinate yCoordinate ->
            ( { model
                | users =
                    case model.selectedUser of
                        Nothing ->
                            model.users

                        Just user ->
                            List.map
                                (\candidateUser ->
                                    if candidateUser == user then
                                        { user | x = xCoordinate, y = yCoordinate }

                                    else
                                        candidateUser
                                )
                                model.users
              }
            , Cmd.none
            )

        ChangeColor xCoordinate yCoordinate ->
            ( { model
                | world =
                    case Array.get yCoordinate world of
                        Nothing ->
                            World world

                        Just row ->
                            case Array.get xCoordinate row of
                                Nothing ->
                                    World world

                                Just color ->
                                    World (Array.set yCoordinate (Array.set xCoordinate (nextColor color) row) world)
              }
            , Cmd.none
            )

        Select user ->
            ( { model | selectedUser = Just user }, Cmd.none )

        SetSomeRandom ->
            ( model, Cmd.none )


view : Model -> Browser.Document Msg
view model =
    { title = "Virtual RC 3.0"
    , body =
        [ viewWorld model.world
        , div [] (List.map (viewUser model.selectedUser) model.users)
        ]
    }


viewUser : Maybe User -> User -> Html Msg
viewUser selectedUser user =
    img
        [ src user.profile
        , style "position" "absolute"
        , style "top" ("calc(" ++ String.fromInt user.y ++ "*" ++ cellSize ++ ")")
        , style "left" ("calc(" ++ String.fromInt user.x ++ "*" ++ cellSize ++ ")")
        , style "max-width" cellSize
        , style "max-height" cellSize
        , if selectedUser == Just user then
            style "outline" "solid 1px blue"
            -- all ifs must have an else

          else
            class ""
        , onClick (Select user)
        ]
        []


viewRow : Int -> Array Color -> Html Msg
viewRow yCoordinate row =
    div
        [ style "display" "flex"
        , style "margin" "0"
        , style "padding" "0"
        ]
        (Array.toList (Array.indexedMap (viewCell yCoordinate) row))


viewWorld : World -> Html Msg
viewWorld (World entries) =
    div
        [ style "margin" "0"
        , style "padding" "0"
        ]
        (Array.toList (Array.indexedMap viewRow entries))


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

                Orange ->
                    "orange"

                Violet ->
                    "violet"

                Indigo ->
                    "indigo"

                Green ->
                    "green"

                Yellow ->
                    "yellow"

                Red ->
                    "red"
    in
    span
        [ style "display" "inline-block"
        , style "background-color" cssColor
        , style "width" cellSize
        , style "outline" "solid 1px lightgray"
        , style "height" cellSize
        , style "margin" "0"
        , style "padding" "0"
        , onClick (GoToRoom xCoordinate yCoordinate)
        , onDoubleClick (ChangeColor xCoordinate yCoordinate)
        ]
        []


cellSize : String
cellSize =
    "30px"


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none
