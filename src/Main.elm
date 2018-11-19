port module Main exposing (..)

import Browser exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Time exposing (..)
import Array exposing (..)
import Debug exposing (..)
import Task exposing (..)
import Json.Encode as Encode
import Json.Decode as Decode
import Markdown exposing (..)


{--

TODO:
- delete
- style memory


GLOSSARY:
entry
memory

--}


main =
  Browser.document { init = init, view = view, update = update, subscriptions = subscriptions }


toEnglishMonth : Time.Month -> String
toEnglishMonth month =
  case month of
    Time.Oct -> "October"
    Time.Nov -> "November"
    _ -> "Some month"


type alias Model =
  { entries : Array Entry
  }


type alias Memory =
  { createdAt : Posix
  , updatedAt : Posix
  , title : Maybe String
  , content : String
  }


memoryToTextValue : Memory -> String
memoryToTextValue memory =
  let
      createdAt : String
      createdAt =
        memory.createdAt
        |> Time.posixToMillis
        |> String.fromInt

      updatedAt : String
      updatedAt =
        memory.updatedAt
        |> Time.posixToMillis
        |> String.fromInt
  in
      [ Just createdAt
      , Just updatedAt
      , memory.title
      , Just memory.content
      ]
      |> List.filterMap identity
      |> String.join "\n"


decodeMemory : String -> Memory
decodeMemory blob =
  let
      lines : Array String 
      lines = 
        blob
        |> String.split "\n"
        |> Array.fromList

      createdAt : Posix
      createdAt =
        Array.get 0 lines
        |> Maybe.andThen String.toInt
        |> Maybe.withDefault 0
        |> Time.millisToPosix

      updatedAt : Posix
      updatedAt =
        Array.get 1 lines
        |> Maybe.andThen String.toInt
        |> Maybe.withDefault 0
        |> Time.millisToPosix

      (title, content) =
        if (Array.length lines >= 4)
        then
          ( lines
            |> Array.get 2
          , lines
            |> Array.slice 3 (Array.length lines)
            |> toList
            |> String.join "\n"
          )
        else
          ( Nothing
          , lines
            |> Array.get 2
            |> Maybe.withDefault ""
          )
  in
      Memory
        createdAt
        updatedAt
        title
        content


type alias Entry = 
  { isEditing : Bool
  , memory : Memory
  }


type Msg
  = Edit Int Posix
  | Save Int
  | Revise Int String
  | Add Posix
  | GetTimeThenEdit Int
  | GetTimeThenAdd


port cache : Encode.Value -> Cmd msg


type alias Flags = Decode.Value


decodeEntries : Flags -> Array Entry
decodeEntries json =
  let
      timeDecoder : Decode.Decoder Time.Posix
      timeDecoder =
        Decode.map Time.millisToPosix Decode.int

      memoryDecoder : Decode.Decoder Memory
      memoryDecoder =
        Decode.map4
          Memory
          (Decode.field "createdAt" timeDecoder)
          (Decode.field "updatedAt" timeDecoder)
          (Decode.field "title" (Decode.nullable Decode.string))
          (Decode.field "content" Decode.string)
  in
      json
      |> Decode.decodeValue (Decode.array memoryDecoder)
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
          []
          [ button [onClick GetTimeThenAdd] [text "Add"]
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
        then renderWritable i entry.memory
        else renderReadable i entry.memory

      renderReadable : Int -> Memory -> Html Msg
      renderReadable i memory =
        let
            title : Maybe String
            title = 
              memory.title
              |> Maybe.map ((++) "# ")

            month : String
            month =
              memory.createdAt
              |> Time.toMonth Time.utc
              |> toEnglishMonth

            day : String
            day =
              memory.createdAt
              |> Time.toDay Time.utc
              |> String.fromInt

            year : String
            year =
              memory.createdAt
              |> Time.toYear Time.utc
              |> String.fromInt

            createdAt : String
            createdAt =
              "## " ++ month ++ " " ++ day ++ ", " ++ year

            blob : String
            blob =
              [ title
              , Just createdAt
              , Just memory.content
              ]
              |> List.filterMap identity
              |> String.join "\n"
        in
            div []
              [ Markdown.toHtml [] blob 
              , button [onClick (GetTimeThenEdit i)] [text "Edit"]
              ]

      renderWritable : Int -> Memory -> Html Msg
      renderWritable i memory =
        div []
        [ textarea
          [ value <| memoryToTextValue memory
          , onInput (Revise i)
          , style "height" "300px"
          , style "width" "100%"
          ]
          []
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
    GetTimeThenEdit i ->
      ( model
      , Task.perform (Edit i) (Time.now)
      )
    Edit i time ->
      let
          target : Maybe Entry
          target = Array.get i model.entries

          updateTime : Memory -> Memory
          updateTime memory =
            { memory | updatedAt = time }

          next : Maybe Entry
          next =
            target
            |> Maybe.map (\t -> { t | isEditing = True, memory = updateTime t.memory })
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

          encodeMemories : Array Memory -> Encode.Value
          encodeMemories memories =
            Encode.array
              encodeMemory
              memories

          encodeMemory : Memory -> Encode.Value
          encodeMemory memory =
            Encode.object
              [ ("createdAt", Encode.int <| Time.posixToMillis memory.createdAt)
              , ("updatedAt", Encode.int <| Time.posixToMillis memory.updatedAt)
              , ( "title"
                ,  memory.title
                  |> Maybe.map Encode.string
                  |> Maybe.withDefault Encode.null
                )
              , ("content", Encode.string memory.content)
              ]
      in
          next
          |> Maybe.map (\n -> Array.set i n model.entries)
          |> Maybe.withDefault model.entries
          |> \entries -> 
            ({ model | entries = entries }
            , cache <| encodeMemories (Array.map .memory entries)
            )
    Revise i newMemory ->
      let
          target : Maybe Entry
          target = Array.get i model.entries

          next : Maybe Entry
          next =
            target
            |> Maybe.map (\t -> { t | memory = decodeMemory newMemory })
      in
          next
          |> Maybe.map (\n -> Array.set i n model.entries)
          |> Maybe.withDefault model.entries
          |> \entries -> 
            ({ model | entries = entries }
            , Cmd.none
            )
    GetTimeThenAdd ->
      ( model
      , Task.perform (Add) (Time.now)
      )
    Add time ->
      let
          newMemory : Memory
          newMemory =
            Memory
              time
              (Time.millisToPosix 0)
              Nothing
              ""
      in
          ({ model | entries = Array.push (Entry True newMemory) model.entries }
          , Cmd.none
          )


subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none
