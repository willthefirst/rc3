module Frontend exposing (..)

-- Our imports

import Array exposing (Array)
import Browser exposing (UrlRequest(..))
import Browser.Navigation as Nav
import Html exposing (..)
import Html.Attributes as Attr exposing (..)
import Html.Events exposing (onClick, onDoubleClick)
import Lamdera
import Types exposing (..)
import Url


type alias Model =
    FrontendModel


app =
    Lamdera.frontend
        { init = init
        , onUrlRequest = UrlClicked
        , onUrlChange = UrlChanged
        , update = update
        , updateFromBackend = updateFromBackend
        , subscriptions = \m -> Sub.none
        , view = view
        }


users : List User
users =
    [ User "https://kofi.sexy/images/profile.jpg" 0 0
    , User "https://raw.githubusercontent.com/captn3m0/avatars/master/gravatar/cuber.jpg" 1 2
    , User "https://d29xw0ra2h4o4u.cloudfront.net/assets/people/justin_holzmann_150-99cc8a73d0752979e3e6900e31c6e4a051fc2e5856920a87b58f26e30db8aa7b.jpg" 2 2
    , User "https://d29xw0ra2h4o4u.cloudfront.net/assets/people/raunak_singh_150-121093425a4819b4f050b1076621a5288c310b29787c0e5ff7774d95a02363b7.jpg" 2 1
    ]


init : Url.Url -> Nav.Key -> ( Model, Cmd FrontendMsg )
init url key =
    ( { key = key
      , message = "Welcome to Lamdera! You're looking at the auto-generated base implementation. Check out src/Frontend.elm to start coding!"
      , world = World (Array.fromList [])
      , users = users
      , selectedUser = Nothing
      , clientId = ""
      }
    , Cmd.none
    )


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


update : FrontendMsg -> Model -> ( Model, Cmd FrontendMsg )
update msg model =
    let
        (World world) =
            model.world
    in
    case msg of
        UrlClicked urlRequest ->
            case urlRequest of
                Internal url ->
                    ( model
                    , Cmd.batch [ Nav.pushUrl model.key (Url.toString url) ]
                    )

                External url ->
                    ( model
                    , Nav.load url
                    )

        UrlChanged url ->
            ( model, Cmd.none )

        NoOpFrontendMsg ->
            ( model, Cmd.none )

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


updateFromBackend : ToFrontend -> Model -> ( Model, Cmd FrontendMsg )
updateFromBackend msg model =
    case msg of
        UpdateWorld newWorld clientId ->
            ( { model | world = newWorld, clientId = clientId }, Cmd.none )

        NoOpToFrontend ->
            ( model, Cmd.none )


view : Model -> Browser.Document FrontendMsg
view model =
    { title = "ASDASD"
    , body =
        [ viewWorld model.world
        , div [] (List.map (viewUser model.selectedUser) model.users)
        ]
    }


viewUser : Maybe User -> User -> Html FrontendMsg
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


viewRow : Int -> Array Color -> Html FrontendMsg
viewRow yCoordinate row =
    div
        [ style "display" "flex"
        , style "margin" "0"
        , style "padding" "0"
        ]
        (Array.toList (Array.indexedMap (viewCell yCoordinate) row))


viewWorld : World -> Html FrontendMsg
viewWorld (World entries) =
    div
        [ style "margin" "0"
        , style "padding" "0"
        ]
        (Array.toList (Array.indexedMap viewRow entries))


viewCell : Int -> Int -> Color -> Html FrontendMsg
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
    "100px"
