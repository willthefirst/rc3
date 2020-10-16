module Backend exposing (..)

import Html
import Lamdera exposing (ClientId, SessionId, broadcast, sendToFrontend)
import Types exposing (..)

import Array exposing (Array)


type alias Model =
    BackendModel


app =
    Lamdera.backend
        { init = init
        , update = update
        , updateFromFrontend = updateFromFrontend
        , subscriptions = \m -> Sub.none
        }


init : ( Model, Cmd BackendMsg )
init =
    ( 
        { message = "Hello!"
        , world = (World
            (Array.fromList
                [ Array.fromList [ White, Blue, Black, Orange, Red, Yellow, Green]
                , Array.fromList [ Blue, Black, Blue, Orange, Red, Yellow, Green ]
                , Array.fromList [ Blue, White, Blue, Orange, Red, Yellow, Green ]
                , Array.fromList [ Blue, White, Blue, Orange, Red, Yellow, Green ]
                , Array.fromList [ Blue, White, Blue, Orange, Red, Yellow, Green ]
                , Array.fromList [ Blue, White, Blue, Orange, Red, Yellow, Green ]
                ]
            )
        )
        , users = []
        }
    , Cmd.none
    )


update : BackendMsg -> Model -> ( Model, Cmd BackendMsg )
update msg model =
    case msg of
        ClientConnected sessionId clientId ->
            Debug.log "client connected" <| ( model, sendToFrontend clientId <| UpdateWorld model.world clientId )

        NoOpBackendMsg ->
            ( model, Cmd.none )


updateFromFrontend : SessionId -> ClientId -> ToBackend -> Model -> ( Model, Cmd BackendMsg )
updateFromFrontend sessionId clientId msg model =
    case msg of
        NoOpToBackend ->
            ( model, Cmd.none )

subscriptions model =
    Sub.batch
        [ Lamdera.onConnect ClientConnected
        ]