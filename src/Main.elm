port module Main exposing (..)

import Browser exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Array exposing (Array)
import Json.Encode
import Json.Decode


{--

TODO:
- save memories
- render memories


GLOSSARY:
entry
memory

--}


main =
  Browser.document { init = init, view = view, update = update, subscriptions = subscriptions }


type alias Model =
  { entries : Array Entry
  }


type alias Memory = String


type alias Entry = 
  { isEditing : Bool
  , memory : Memory
  }


type Msg
  = Edit Int 
  | Save Int
  | Revise Int String
  | Add


port cache : Json.Encode.Value -> Cmd msg


type alias Flags = Json.Decode.Value


decodeEntries : Flags -> Array Entry
decodeEntries json =
  json
  |> Json.Decode.decodeValue (Json.Decode.array Json.Decode.string)
  |> Result.withDefault Array.empty
  |> Array.map (Entry False)


init : Flags -> (Model, Cmd Msg)
init flags =
  ( Model <| decodeEntries flags
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
          , renderEverything
          ]

      renderEverything : Html Msg
      renderEverything =
        div
          [ style "width" "400px"
          , style "margin-left" "auto"
          , style "margin-right" "auto"
          ]
          ( model.entries
            |> Array.indexedMap renderEntry
            |> Array.toList
            |> List.intersperse (hr [] [])
            |> List.reverse
          )

      renderEntry : Int -> Entry -> Html Msg
      renderEntry i entry =
        if entry.isEditing
        then renderWritable i entry
        else renderReadable i entry

      renderReadable : Int -> Entry -> Html Msg
      renderReadable i entry =
        div []
          [ text entry.memory
          , button [onClick (Edit i)] [text "Edit"]
          ]

      renderWritable : Int -> Entry -> Html Msg
      renderWritable i entry =
        div []
        [ textarea [value entry.memory, onInput (Revise i)] []
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
          target : Maybe Entry
          target = Array.get i model.entries

          next : Maybe Entry
          next =
            target
            |> Maybe.map (\t -> { t | isEditing = True })
      in
          next
          |> Maybe.map (\n -> Array.set i n model.entries)
          |> Maybe.withDefault model.entries
          |> \entries -> 
            ({ model | entries = entries }
            , Cmd.none
            )
    Save i -> 
      let
          target : Maybe Entry
          target = Array.get i model.entries

          next : Maybe Entry
          next =
            target
            |> Maybe.map (\t -> { t | isEditing = False })

          encodeEntries : Array Entry -> Json.Encode.Value
          encodeEntries entries =
            Json.Encode.array
              (\entry -> Json.Encode.string entry.memory) -- better way to write this?
              entries
      in
          next
          |> Maybe.map (\n -> Array.set i n model.entries)
          |> Maybe.withDefault model.entries
          |> \entries -> 
            ({ model | entries = entries }
            , cache <| encodeEntries entries
            )
    Revise i newMemory ->
      let
          target : Maybe Entry
          target = Array.get i model.entries

          next : Maybe Entry
          next =
            target
            |> Maybe.map (\t -> { t | memory = newMemory })
      in
          next
          |> Maybe.map (\n -> Array.set i n model.entries)
          |> Maybe.withDefault model.entries
          |> \entries -> 
            ({ model | entries = entries }
            , Cmd.none
            )
    Add ->
      ({ model | entries = Array.push (Entry True "") model.entries }
      , Cmd.none
      )


subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none
