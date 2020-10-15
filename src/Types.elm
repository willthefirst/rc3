module Types exposing (..)

import Browser exposing (UrlRequest)
import Browser.Navigation exposing (Key)
import Url exposing (Url)

-- Our imports
import Array exposing (Array)

type alias User =
    { profile : String
    , x : Int
    , y : Int
    }

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

type World
    = World (Array (Array Color))

type alias FrontendModel =
    { key : Key
    , message : String
    , world : World
    , users : List User
    , selectedUser : Maybe User
    }


type alias BackendModel =
    { message : String
    , world : World
    , users : List User
    }


type FrontendMsg
    = UrlClicked UrlRequest
    | UrlChanged Url
    | ChangeColor Int Int -- X and Y coordinates in the grid.
    | GoToRoom Int Int -- X and Y coordinates in the grid.
    | Select User
    | NoOpFrontendMsg


type ToBackend
    = NoOpToBackend


type BackendMsg
    = NoOpBackendMsg


type ToFrontend
    = NoOpToFrontend
