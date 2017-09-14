module Entry exposing
    ( Entry
    , markEntryWithId, sumMarkedPoints
    , viewEntryList, getEntries
    )

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import Http
import Json.Decode as Decode exposing (Decoder, field, succeed)
import Json.Decode.Pipeline as DecodePipeline exposing (decode, required, optional, hardcoded)

type alias Entry =
    { id : Int
    , phrase : String
    , points : Int
    , marked : Bool
    }

markEntryWithId : List Entry -> Int -> List Entry
markEntryWithId entries id =
    let
        markEntry e =
            if e.id == id then
                { e | marked = (not e.marked) }
            else
                e
    in
    List.map markEntry entries

entryDecoder : Decoder Entry
entryDecoder =
    decode Entry
        |> DecodePipeline.required "id" Decode.int
        |> DecodePipeline.required "phrase" Decode.string
        |> optional "points" Decode.int 100
        |> hardcoded False

entryListDecoder : Decoder (List Entry)
entryListDecoder =
    Decode.list entryDecoder

getEntries : (Result Http.Error (List Entry) -> msg) -> String -> Cmd msg
getEntries msg url =
    entryListDecoder
        |> Http.get url
        |> Http.send msg

viewEntryItem : (Int -> msg) -> Entry -> Html msg
viewEntryItem msg entry =
    li [ classList [ ("marked", entry.marked) ], onClick (msg entry.id) ]
        [ span [ class "phrase" ] [ text entry.phrase ]
        , span [ class "points" ] [ text (toString entry.points) ]
        ]

viewEntryList : (Int -> msg) -> List Entry -> Html msg
viewEntryList msg entries =
    entries
        |> List.map (viewEntryItem msg)
        |> ul []

sumMarkedPoints : List Entry -> Int
sumMarkedPoints entries =
    entries
        |> List.filter .marked
        |> List.foldl (\e sum -> sum + e.points) 0
