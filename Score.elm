module Score exposing (Score, viewScore, postScore, hasZeroScore)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import Http
import Json.Decode as Decode exposing (Decoder, field, succeed)
import Json.Decode.Pipeline as DecodePipeline exposing (decode, required, optional, hardcoded)
import Json.Encode as Encode

import Entry

type alias Score =
    { id : Int
    , name : String
    , score : Int
    }

scoreDecoder : Decoder Score
scoreDecoder =
    decode Score
        |> DecodePipeline.required "id" Decode.int
        |> DecodePipeline.required "name" Decode.string
        |> DecodePipeline.required "score" Decode.int

encodeScore : { a | entries : List Entry.Entry, name : String } -> Encode.Value
encodeScore model =
    Encode.object
        [ ("name", Encode.string model.name)
        , ("score", Encode.int (Entry.sumMarkedPoints model.entries))
        ]

postScore : (Result Http.Error Score -> msg) -> String -> { a | entries : List Entry.Entry, name : String } -> Cmd msg
postScore msg url model =
    let
        body =
            encodeScore model
                |> Http.jsonBody

        request =
            Http.post url body scoreDecoder
    in
        Http.send msg request

hasZeroScore : { a | entries : List Entry.Entry, name : String } -> Bool
hasZeroScore model =
  (Entry.sumMarkedPoints model.entries) == 0

viewScore : Int -> Html msg
viewScore sum =
    div
        [ class "score" ]
        [ span [ class "label" ] [ text "Score" ]
        , span [ class "value" ] [ text (toString sum) ]
        ]
