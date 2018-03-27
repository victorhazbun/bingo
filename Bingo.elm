module Bingo exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import Random
import Http
import Json.Decode as Decode exposing (int, string, float, Decoder)
import Json.Encode as Encode
import Json.Decode.Pipeline as JsonDecodePipeline exposing (decode, required, optional, hardcoded)
import ViewHelpers exposing (..)
import Entry as BingoEntry


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


type alias Score =
    { id : Int
    , name : String
    , score : Int
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
    | NewScore (Result Http.Error Score)
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
            ( model, postScore model )

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


httpErrorToMessage : Http.Error -> String
httpErrorToMessage error =
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



-- DECODERS/ENCODERS


scoreDecoder : Decoder Score
scoreDecoder =
    JsonDecodePipeline.decode Score
        |> JsonDecodePipeline.required "id" int
        |> JsonDecodePipeline.required "name" string
        |> JsonDecodePipeline.required "score" int


encodeScore : Model -> Encode.Value
encodeScore model =
    Encode.object
        [ ( "name", Encode.string model.name )
        , ( "score", Encode.int (BingoEntry.sumMarkedPoints model.entries) )
        ]



-- COMMANDS
-- In Elm, a command is a set of instructions. And the Elm Runtime is the perfect Ikea furniture assembler.


apiUrlPrefix : String
apiUrlPrefix =
    "http://localhost:3000"


generateRandomNumber : Cmd Msg
generateRandomNumber =
    Random.generate NewRandom (Random.int 1 100)


postScore : Model -> Cmd Msg
postScore model =
    let
        url =
            (apiUrlPrefix ++ "/scores")

        body =
            encodeScore model
                |> Http.jsonBody

        request =
            Http.request
                { method = "POST"
                , headers = []
                , url = url
                , body = body
                , expect = Http.expectJson scoreDecoder
                , timeout = Nothing
                , withCredentials = False
                }
    in
        Http.send NewScore request


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


viewScore : Int -> Html Msg
viewScore sum =
    div [ class "score" ]
        [ span [ class "label" ] [ text "Score" ]
        , span [ class "value" ] [ text (toString sum) ]
        ]


hasZeroScore : Model -> Bool
hasZeroScore model =
    (BingoEntry.sumMarkedPoints model.entries) == 0


viewScoreButtonText : Model -> String
viewScoreButtonText model =
    if (hasZeroScore model) then
        "Play to share"
    else
        "Share Score"


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
        , viewScore (BingoEntry.sumMarkedPoints model.entries)
        , div [ class "button-group" ]
            [ primaryButton NewGame "New Game" Nothing Nothing
            , primaryButton Sort "Sort" Nothing Nothing
            , primaryButton ShareScore (viewScoreButtonText model) (Just (hasZeroScore model)) (Just [ ( "disabled", hasZeroScore model ) ])
            ]

        --, div [ class "debug" ] [ text (toString model) ]
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
