module Main exposing (Msg(..), main, update, view)

import Array exposing (Array)
import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onDoubleClick)


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


type World
    = World (Array (Array Location))


type Location
    = NotWall Color -- Rename this type blah
    | Wall


type alias Model =
    { world : World
    , users : List User
    , selectedUser : Maybe User
    }


init : Model
init =
    Model
        (World
            (Array.fromList
                [ Array.fromList [ NotWall White, NotWall Blue, NotWall Black, NotWall Orange, NotWall Orange, NotWall Yellow, NotWall Green ]
                , Array.fromList [ NotWall Blue, NotWall Black, NotWall Blue, NotWall Orange, NotWall Orange, Wall, Wall ]
                , Array.fromList [ NotWall Blue, NotWall White, NotWall Blue, NotWall Orange, NotWall Orange, Wall, NotWall Green ]
                ]
            )
        )
        users
        Nothing


main =
    Browser.sandbox { init = init, update = update, view = view }


type Msg
    = ChangeColor Int Int -- X and Y coordinates in the grid.
    | GoToCoordinate Int Int -- X and Y coordinates in the grid.
    | Select User


update : Msg -> Model -> Model
update msg model =
    let
        (World world) =
            model.world
    in
    case msg of
        GoToCoordinate xCoordinate yCoordinate ->
            { model
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

        ChangeColor xCoordinate yCoordinate ->
            { model
                | world =
                    case Array.get yCoordinate world of
                        Nothing ->
                            World world

                        Just row ->
                            case Array.get xCoordinate row of
                                Nothing ->
                                    World world

                                Just loc ->
                                    case loc of
                                        NotWall color ->
                                            World (Array.set yCoordinate (Array.set xCoordinate (NotWall (nextColor color)) row) world)

                                        Wall ->
                                            World world
            }

        Select user ->
            { model | selectedUser = Just user }


view : Model -> Html Msg
view model =
    div []
        [ viewWorld model.world
        , div [] (List.map (viewUser model.selectedUser) model.users)
        ]


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


viewWorld : World -> Html Msg
viewWorld (World entries) =
    div
        [ style "margin" "0"
        , style "padding" "0"
        ]
        (Array.toList (Array.indexedMap viewRow entries))


viewRow : Int -> Array Location -> Html Msg
viewRow yCoordinate row =
    div
        [ style "display" "flex"
        , style "margin" "0"
        , style "padding" "0"
        ]
        (Array.toList (Array.indexedMap (viewCell yCoordinate) row))


viewCell : Int -> Int -> Location -> Html Msg
viewCell yCoordinate xCoordinate location =
    let
        cssColor loc =
            case loc of
                NotWall c ->
                    unwrapColor c

                Wall ->
                    "gray"
    in
    span
        [ style "display" "inline-block"
        , style "background-color" (cssColor location)
        , style "width" cellSize
        , style "outline" "solid 1px lightgray"
        , style "height" cellSize
        , style "margin" "0"
        , style "padding" "0"
        , onClick (GoToCoordinate xCoordinate yCoordinate)
        , onDoubleClick (ChangeColor xCoordinate yCoordinate)
        ]
        []


cellSize : String
cellSize =
    "100px"


unwrapColor : Color -> String
unwrapColor color =
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
