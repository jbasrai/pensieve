import Browser exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Array exposing (Array)


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
  { memories : Array Memory
  }


type alias Memory = 
  { content : String
  , isEditing : Bool
  }


type Msg
  = Edit Int 
  | Save Int
  | Revise Int String


lyrics : List String
lyrics =
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


init : () -> (Model, Cmd Msg)
init _ =
  ( Model (Array.fromList <| List.map (\x -> Memory x False) lyrics)
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
          |> Array.indexedMap renderMemory
          |> Array.toList
          |> List.intersperse (hr [] []))

      renderMemory : Int -> Memory -> Html Msg
      renderMemory i memory =
        if memory.isEditing
        then renderEdit i memory
        else renderView i memory

      renderView : Int -> Memory -> Html Msg
      renderView i memory =
        div []
          [ text memory.content
          , button [onClick (Edit i)] [text "Edit"]
          ]

      renderEdit : Int -> Memory -> Html Msg
      renderEdit i memory =
        div []
        [ textarea [value memory.content, onInput (Revise i)] []
        , button [onClick (Save i)] [text "Save"]
        ]
  in
      Document
        "pensieve"
        [ renderPage
        ]


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Edit i ->
      let
          target : Maybe Memory
          target = Array.get i model.memories

          next : Maybe Memory
          next =
            target
            |> Maybe.map (\t -> { t | isEditing = True })
      in
          next
          |> Maybe.map (\n -> Array.set i n model.memories)
          |> Maybe.withDefault model.memories
          |> \memories -> 
            ({ model | memories = memories }
            , Cmd.none
            )
    Save i -> 
      let
          target : Maybe Memory
          target = Array.get i model.memories

          next : Maybe Memory
          next =
            target
            |> Maybe.map (\t -> { t | isEditing = False })
      in
          next
          |> Maybe.map (\n -> Array.set i n model.memories)
          |> Maybe.withDefault model.memories
          |> \memories -> 
            ({ model | memories = memories }
            , Cmd.none
            )
    Revise i newContent ->
      let
          target : Maybe Memory
          target = Array.get i model.memories

          next : Maybe Memory
          next =
            target
            |> Maybe.map (\t -> { t | content = newContent })
      in
          next
          |> Maybe.map (\n -> Array.set i n model.memories)
          |> Maybe.withDefault model.memories
          |> \memories -> 
            ({ model | memories = memories }
            , Cmd.none
            )


subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none
