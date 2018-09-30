port module Main exposing (..)

import Browser exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Array exposing (Array)
import Json.Encode
import Json.Decode


{--
TODO

- render memories
- edit memories
- add memories
- persist to local storage

persisting data locally is the first step, but also want to leave room for 1) remotely / distributedly recording memories and 2) exporting / sharing and sharing memories

--}


main =
  Browser.document { init = init, view = view, update = update, subscriptions = subscriptions }


type alias Model =
  { memories : Array Memory
  }


type alias Memory = 
  { isEditing : Bool
  , content : String
  }


type Msg
  = Edit Int 
  | Save Int
  | Revise Int String
  | Add


port cache : Json.Encode.Value -> Cmd msg


type alias Flags = Json.Decode.Value


decodeMemories : Flags -> Array Memory
decodeMemories json =
  json
  |> Json.Decode.decodeValue (Json.Decode.array Json.Decode.string)
  |> Result.withDefault Array.empty
  |> Array.map (Memory False)


init : Flags -> (Model, Cmd Msg)
init flags =
  ( Model <| decodeMemories flags
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
          [ button [onClick Add] [text "Add"]
          , renderContent
          ]

      renderContent : Html Msg
      renderContent =
        div
          [ style "width" "400px"
          , style "margin-left" "auto"
          , style "margin-right" "auto"
          ]
          ( model.memories
            |> Array.indexedMap renderMemory
            |> Array.toList
            |> List.intersperse (hr [] [])
            |> List.reverse
          )

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

          encodeMemories : Array Memory -> Json.Encode.Value
          encodeMemories memories =
            Json.Encode.array
              (\memory -> Json.Encode.string memory.content) -- better way to write this?
              memories
      in
          next
          |> Maybe.map (\n -> Array.set i n model.memories)
          |> Maybe.withDefault model.memories
          |> \memories -> 
            ({ model | memories = memories }
            , cache <| encodeMemories memories
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
    Add ->
      ({ model | memories = Array.push (Memory True "") model.memories }
      , Cmd.none
      )


subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none
