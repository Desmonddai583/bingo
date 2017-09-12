module Bingo exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Random
import Http
import Json.Decode as Decode exposing (Decoder, field, succeed)
import Json.Decode.Pipeline as DecodePipeline exposing (decode, required, optional, hardcoded)

-- MODEL

type alias Model =
    { name : String
    , gameNumber : Int
    , entries : List Entry
    , alertMessage : Maybe String
    }

type alias Entry =
    { id : Int
    , phrase : String
    , points : Int
    , marked : Bool
    }

initialModel : Model
initialModel =
    { name = "Desmond"
    , gameNumber = 1
    , entries = []
    , alertMessage = Nothing
    }

-- UPDATE

type Msg
    = NewGame
    | Mark Int
    | Sort
    | NewRandom Int
    | NewEntries (Result Http.Error (List Entry))
    | CloseAlert

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NewRandom randomNumber ->
            ( { model | gameNumber = randomNumber }, Cmd.none )
        NewGame ->
            ( { model | gameNumber = model.gameNumber + 1 }, getEntries )
        NewEntries (Ok randomEntries) ->
            ( { model | entries = List.sortBy .points randomEntries }, Cmd.none )
        NewEntries (Err error) ->
            let
                errorMessage =
                    case error of
                        Http.NetworkError ->
                            "Is the server running?"

                        Http.Timeout ->
                            "Request timed out!"

                        Http.BadUrl url ->
                            ("Invalid URL: " ++ url)

                        Http.BadStatus response ->
                            case response.status.code of
                                401 ->
                                    "Unauthorized"
                                404 ->
                                    "Not Found"
                                code ->
                                    (toString code)

                        Http.BadPayload reason response ->
                            reason
            in
                ( { model | alertMessage = Just errorMessage }, Cmd.none )
        CloseAlert ->
            ( { model | alertMessage = Nothing }, Cmd.none )
        Mark id ->
            let
                markEntry e =
                    if e.id == id then
                        { e | marked = (not e.marked) }
                    else
                        e
            in
                ( { model | entries = List.map markEntry model.entries }, Cmd.none )
        Sort ->
            ( { model | entries = List.sortBy .points model.entries }, Cmd.none )


-- DECODERS

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

-- COMMANDS

generateRandomNumber : Cmd Msg
generateRandomNumber =
    Random.generate NewRandom (Random.int 1 100)

entriesUrl : String
entriesUrl =
    "http://localhost:3001/random-entries"

getEntries : Cmd Msg
getEntries =
    entryListDecoder
        |> Http.get entriesUrl
        |> Http.send NewEntries

-- VIEW

playerInfo : String -> Int -> String
playerInfo name gameNumber =
    name ++ " - Game #" ++ (toString gameNumber)

viewPlayer : String -> Int -> Html Msg
viewPlayer name gameNumber =
    let
        playerInfoText =
            playerInfo name gameNumber
                |> String.toUpper
                |> text
    in
        h2 [ id "info", class "classy" ]
            [ playerInfoText ]

viewHeader : String -> Html Msg
viewHeader title =
    header []
        [ h1 [] [ text title ] ]

viewFooter : Html Msg
viewFooter =
    footer []
        [ a [ href "http://elm-lang.org" ]
            [ text "Powered By Elm" ]
        ]

viewEntryItem : Entry -> Html Msg
viewEntryItem entry =
    li [ classList [ ("marked", entry.marked) ], onClick (Mark entry.id) ]
        [ span [ class "phrase" ] [ text entry.phrase ]
        , span [ class "points" ] [ text (toString entry.points) ]
        ]

viewEntryList : List Entry -> Html Msg
viewEntryList entries =
    entries
        |> List.map viewEntryItem
        |> ul []

allEntriesMarked : List Entry -> Bool
allEntriesMarked entries =
    List.all .marked entries

sumMarkedPoints : List Entry -> Int
sumMarkedPoints entries =
    entries
        |> List.filter .marked
        |> List.foldl (\e sum -> sum + e.points) 0

viewScore : Int -> Html Msg
viewScore sum =
    div
        [ class "score" ]
        [ span [ class "label" ] [ text "Score" ]
        , span [ class "value" ] [ text (toString sum) ]
        ]

view : Model -> Html Msg
view model =
    div [ class "content" ]
        [ viewHeader "BUZZWORD BINGO"
        , viewPlayer model.name model.gameNumber
        , viewAlertMessage model.alertMessage
        , viewEntryList model.entries
        , viewScore (sumMarkedPoints model.entries)
        , div [ class "button-group" ]
                [ button [ onClick NewGame ] [ text "New Game" ]
                , button [ onClick Sort ] [ text "Sort" ]
                ]
        , div [ class "debug" ] [ text (toString model) ]
        , viewFooter
        ]

viewAlertMessage : Maybe String -> Html Msg
viewAlertMessage alertMessage =
    case alertMessage of
        Just message ->
            div [ class "alert" ]
                [ span [ class "close", onClick CloseAlert ] [ text "X" ]
                , text message
                ]
        Nothing ->
            text ""

main : Program Never Model Msg
main =
    program
        { init = ( initialModel, getEntries )
        , view = view
        , update = update
        , subscriptions = (\_ -> Sub.none)
        }
