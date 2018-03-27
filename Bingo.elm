module Bingo exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import Random
import Http
import ViewHelpers exposing (..)
import ErrorHandlerHelpers exposing (..)
import Entry as BingoEntry
import Score as BingoScore


-- MODEL


type GameState
    = EnteringName
    | Playing


type alias Model =
    { name : String
    , gameNumber : Int
    , entries : List BingoEntry.Entry
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
    | NewEntries (Result Http.Error (List BingoEntry.Entry))
    | CloseAlert
    | ShareScore
    | NewScore (Result Http.Error BingoScore.Score)
    | SetNameInput String
    | SaveName
    | CancelName
    | ChangeGameState GameState


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ChangeGameState state ->
            { model | gameState = state } ! [ Cmd.none ]

        SaveName ->
            if String.isEmpty model.nameInput then
                model ! [ Cmd.none ]
            else
                { model
                    | name = model.nameInput
                    , nameInput = ""
                    , gameState = Playing
                }
                    ! [ Cmd.none ]

        CancelName ->
            { model
                | nameInput = ""
                , gameState = Playing
            }
                ! [ Cmd.none ]

        SetNameInput value ->
            { model | nameInput = value } ! [ Cmd.none ]

        NewRandom randomNumber ->
            { model | gameNumber = randomNumber } ! [ Cmd.none ]

        ShareScore ->
            ( model, postScore model.name (BingoEntry.sumMarkedPoints model.entries) )

        NewScore result ->
            case result of
                Ok score ->
                    let
                        message =
                            "Your score of "
                                ++ (toString score.score)
                                ++ " was successfully shared!"
                    in
                        ( { model | alertMessage = Just message }, Cmd.none )

                Err error ->
                    { model | alertMessage = Just (httpErrorToMessage error) } ! [ Cmd.none ]

        NewGame ->
            model ! [ generateRandomNumber, getEntries ]

        NewEntries result ->
            case result of
                Ok randomEntries ->
                    ( { model | entries = randomEntries }, Cmd.none )

                Err error ->
                    { model | alertMessage = Just (httpErrorToMessage error) } ! [ Cmd.none ]

        CloseAlert ->
            { model | alertMessage = Nothing } ! [ Cmd.none ]

        Mark id ->
            { model | entries = BingoEntry.markEntryWithId id model.entries } ! [ Cmd.none ]

        Sort ->
            let
                sortByPoints entry1 entry2 =
                    compare entry2.points entry1.points
            in
                { model | entries = List.sortWith sortByPoints model.entries } ! [ Cmd.none ]



-- COMMANDS
-- In Elm, a command is a set of instructions. And the Elm Runtime is the perfect Ikea furniture assembler.


apiUrlPrefix : String
apiUrlPrefix =
    "http://localhost:3000"


generateRandomNumber : Cmd Msg
generateRandomNumber =
    Random.generate NewRandom (Random.int 1 100)


postScore : String -> Int -> Cmd Msg
postScore name score =
    BingoScore.postScore NewScore (apiUrlPrefix ++ "/scores") name score


getEntries : Cmd Msg
getEntries =
    BingoEntry.getEntries NewEntries (apiUrlPrefix ++ "/random-entries")



-- VIEW


viewPlayer : String -> Int -> Html Msg
viewPlayer name gameNumber =
    h2 [ id "info", class "classy" ]
        [ a [ href "#", onClick (ChangeGameState EnteringName) ]
            [ text name ]
        , text (" - Game #" ++ (toString gameNumber))
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
                    , value model.nameInput
                    , onInput SetNameInput
                    ]
                    []
                , primaryButton SaveName "Save" Nothing Nothing
                , primaryButton CancelName "Cancel" Nothing Nothing
                ]

        Playing ->
            text ""


view : Model -> Html Msg
view model =
    div [ class "content" ]
        [ viewHeader "BUZZWORD BINGO"
        , viewPlayer model.name model.gameNumber
        , alert CloseAlert model.alertMessage
        , viewNameInput model
        , BingoEntry.viewEntryList Mark model.entries
        , BingoScore.viewScore (BingoEntry.sumMarkedPoints model.entries)
        , div [ class "button-group" ]
            [ primaryButton NewGame "New Game" Nothing Nothing
            , primaryButton Sort "Sort" Nothing Nothing
            , primaryButton ShareScore (BingoScore.viewScoreButtonText model.entries) (Just (BingoScore.hasZeroScore model.entries)) (Just [ ( "disabled", BingoScore.hasZeroScore model.entries ) ])
            ]
        , viewFooter
        ]


main : Program Never Model Msg
main =
    Html.program
        { init = ( initialModel, getEntries )
        , view = view
        , update = update
        , subscriptions = (always Sub.none)
        }
