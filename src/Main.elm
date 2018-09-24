import Browser exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)


{--
TODO

- render memories
- edit memories
- add memories
- persist to local storage
--}


main =
  Browser.document { init = init, view = view, update = update, subscriptions = subscriptions }


type alias Model =
  { memories : List Memory
  }


type alias Memory = String


type alias Msg = Int


init : () -> (Model, Cmd Msg)
init _ =
  ( Model
      [ 
        """
        My eyelids float
        Between two states
        On the borderline
        Flows a slow intake
        They don't let much in
        I don't let much out
        Just a broken bulb
        Flickering with doubt
        """,

        """
        Misery loves me
        But I don't love her
        It's a one-way street
        Circling planet earth
        If I take a right
        Maybe I'll outrun
        To the great unknown
        Goes the great no one
        """,

        """
        Wanna move
        I do I do I do
        I'm trying to get this right
        But I'm losing the appetite
        It's not the way I'd choose I'd choose I'd choose
        Always caught in between
        The ache and the apathy
        """
      ]
  , Cmd.none
  )


view : Model -> Document Msg
view model =
  let
      renderPage : Html Msg
      renderPage =
        div
          [ style "backgroundColor" "beige"
          ]
          [ renderContent
          ]

      renderContent : Html Msg
      renderContent =
        div
          [ style "width" "400px"
          , style "margin-left" "auto"
          , style "margin-right" "auto"
          ]
          (model.memories
          |> List.map renderMemory
          |> List.intersperse (hr [] []))

      renderMemory : Memory -> Html Msg
      renderMemory memory =
        div [] [text memory]
  in
      Document
        "pensieve"
        [ renderPage
        ]


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  ( model
  , Cmd.none
  )


subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none
