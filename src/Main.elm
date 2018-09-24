import Browser exposing (..)
import Html exposing (..)


main =
  Browser.document { init = init, view = view, update = update, subscriptions = subscriptions }


type alias Model =
  { text : String
  }


type alias Msg = Int


init : () -> (Model, Cmd Msg)
init _ =
  ( Model "hello"
  , Cmd.none
  )


view : Model -> Document Msg
view model =
  Document
    "pensieve"
    [ div [] [text model.text]
    ]


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  ( model
  , Cmd.none
  )


subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none
