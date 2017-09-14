module Bingo exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import Random
import Http

import ViewHelpers exposing (..)
import Entry
import Score

-- MODEL

type GameState = EnteringName | Playing

type alias Model =
    { name : String
    , gameNumber : Int
    , entries : List Entry.Entry
    , alertMessage : Maybe String
    , nameInput : String
    , gameState : GameState
    }

initialModel : Model
initialModel =
    { name = "Anonymous"
    , gameNumber = 1
    , entries = []
    , alertMessage = Nothing
    , nameInput = ""
    , gameState = EnteringName
    }

-- UPDATE

type Msg
    = NewGame
    | Mark Int
    | Sort
    | NewRandom Int
    | NewEntries (Result Http.Error (List Entry.Entry))
    | CloseAlert
    | ShareScore
    | NewScore (Result Http.Error Score.Score)
    | SetNameInput String
    | SaveName
    | CancelName
    | ChangeGameState GameState

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ChangeGameState state ->
            ( { model | gameState = state }, Cmd.none )

        SaveName ->
            if String.isEmpty model.nameInput then
                ( model, Cmd.none )
            else
                ( { model | name = model.nameInput,
                            nameInput = "",
                            gameState = Playing }, Cmd.none )

        CancelName ->
            ( { model | nameInput = "", gameState = Playing }, Cmd.none )

        SetNameInput value ->
            ( { model | nameInput = value }, Cmd.none )

        NewRandom randomNumber ->
            ( { model | gameNumber = randomNumber }, Cmd.none )

        ShareScore ->
            ( model, Score.postScore NewScore scoreUrl model )

        NewScore (Ok score) ->
            let
                message =
                    "Your score of "
                        ++ (toString score.score)
                        ++ " was successfully shared!"
            in
                ( { model | alertMessage = Just message }, Cmd.none )

        NewScore (Err error) ->
            ( { model | alertMessage = Just (httpErrorMessage error) }, Cmd.none )

        NewGame ->
            ( { model | gameNumber = model.gameNumber + 1 }, getEntries )

        NewEntries (Ok randomEntries) ->
            ( { model | entries = List.sortBy .points randomEntries }, Cmd.none )

        NewEntries (Err error) ->
                ( { model | alertMessage = Just (httpErrorMessage error) }, Cmd.none )

        CloseAlert ->
            ( { model | alertMessage = Nothing }, Cmd.none )

        Mark id ->
            ( { model | entries = Entry.markEntryWithId model.entries id }, Cmd.none )

        Sort ->
            ( { model | entries = List.sortBy .points model.entries }, Cmd.none )

httpErrorMessage : Http.Error -> String
httpErrorMessage error =
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

-- COMMANDS

generateRandomNumber : Cmd Msg
generateRandomNumber =
    Random.generate NewRandom (Random.int 1 100)

apiUrlPrefix : String
apiUrlPrefix =
    "http://localhost:3001"

getEntries : Cmd Msg
getEntries =
    Entry.getEntries NewEntries (apiUrlPrefix ++ "/random-entries")

scoreUrl : String
scoreUrl =
    apiUrlPrefix ++ "/scores"

isNothing : Maybe a -> Bool
isNothing maybe =
    case maybe of
        Just _ ->
            False

        Nothing ->
            True

-- VIEW

viewPlayer : String -> Int -> Html Msg
viewPlayer name gameNumber =
    h2 [ id "info", class "classy" ]
        [ a [href "#", onClick (ChangeGameState EnteringName)]
            [ text name]
        , text (" - Game #" ++ (toString gameNumber))
        ]

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

allEntriesMarked : List Entry.Entry -> Bool
allEntriesMarked entries =
    List.all .marked entries

view : Model -> Html Msg
view model =
    div [ class "content" ]
        [ viewHeader "BUZZWORD BINGO"
        , viewPlayer model.name model.gameNumber
        , alert CloseAlert model.alertMessage
        , viewNameInput model
        , Entry.viewEntryList Mark model.entries
        , Score.viewScore (Entry.sumMarkedPoints model.entries)
        , div [ class "button-group" ]
                [ primaryButton NewGame "New Game" False
                , primaryButton ShareScore "Share Score" (Score.hasZeroScore model)
                , primaryButton Sort "Sort" False
                ]
        , div [ class "debug" ] [ text (toString model) ]
        , viewFooter
        ]

viewNameInput : Model -> Html Msg
viewNameInput model =
    case model.gameState of
        EnteringName ->
            div [ class "name-input" ]
                [ input
                    [ type_ "text"
                    , placeholder "Who's playing?"
                    , autofocus True
                    , onInput SetNameInput
                    , value model.nameInput
                    ]
                    []
                , primaryButton SaveName "Save" False
                , primaryButton CancelName "Cancel" False
                ]
        Playing ->
            text ""

main : Program Never Model Msg
main =
    program
        { init = ( initialModel, getEntries )
        , view = view
        , update = update
        , subscriptions = (\_ -> Sub.none)
        }
