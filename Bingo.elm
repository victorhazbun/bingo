module Bingo exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import Random
import Http
import Json.Decode as Decode exposing (int, string, float, Decoder)
import Json.Encode as Encode
import Json.Decode.Pipeline as JsonDecodePipeline exposing (decode, required, optional, hardcoded)


-- MODEL


type GameState
    = EnteringName
    | Playing


type alias Model =
    { name : String
    , gameNumber : Int
    , entries : List Entry
    , alertMessage : Maybe String
    , nameInput : String
    , gameState : GameState
    }


type alias Entry =
    { id : Int
    , phrase : String
    , points : Int
    , marked : Bool
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
    | NewEntries (Result Http.Error (List Entry))
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
                    let
                        message =
                            "Error posting your score: "
                                ++ (toString error)
                    in
                        ( { model | alertMessage = Just message }, Cmd.none )

        NewGame ->
            model ! [ generateRandomNumber, getEntries ]

        NewEntries result ->
            case result of
                Ok randomEntries ->
                    ( { model | entries = randomEntries }, Cmd.none )

                Err error ->
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
                        { model | alertMessage = Just errorMessage } ! [ Cmd.none ]

        CloseAlert ->
            { model | alertMessage = Nothing } ! [ Cmd.none ]

        Mark id ->
            let
                markEntry e =
                    if e.id == id then
                        { e | marked = (not e.marked) }
                    else
                        e

                markedEntries =
                    model.entries
                        |> List.map markEntry
            in
                { model | entries = markedEntries } ! [ Cmd.none ]

        Sort ->
            let
                sortByPoints entry1 entry2 =
                    compare entry2.points entry1.points
            in
                { model | entries = List.sortWith sortByPoints model.entries } ! [ Cmd.none ]



-- DECODERS/ENCODERS


entryDecoder : Decoder Entry
entryDecoder =
    JsonDecodePipeline.decode Entry
        |> JsonDecodePipeline.required "id" int
        |> JsonDecodePipeline.required "phrase" string
        |> JsonDecodePipeline.optional "points" int 100
        |> JsonDecodePipeline.hardcoded False


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
        , ( "score", Encode.int (sumMarkedPoints model.entries) )
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
    (Decode.list entryDecoder)
        |> Http.get (apiUrlPrefix ++ "/random-entries")
        |> Http.send NewEntries



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


viewEntryItem : Entry -> Html Msg
viewEntryItem entry =
    li [ classList [ ( "marked", entry.marked ) ], onClick (Mark entry.id) ]
        [ span [ class "phrase" ] [ text entry.phrase ]
        , span [ class "points" ] [ text (toString entry.points) ]
        ]


viewEntryList : List Entry -> Html Msg
viewEntryList entries =
    entries
        |> List.map viewEntryItem
        |> ul []


sumMarkedPoints : List Entry -> Int
sumMarkedPoints entries =
    entries
        |> List.filter .marked
        |> List.foldl (\e sum -> sum + e.points) 0


viewScore : Int -> Html Msg
viewScore sum =
    div [ class "score" ]
        [ span [ class "label" ] [ text "Score" ]
        , span [ class "value" ] [ text (toString sum) ]
        ]


hasZeroScore : Model -> Bool
hasZeroScore model =
    (sumMarkedPoints model.entries) == 0


viewScoreButtonText : Model -> Html msg
viewScoreButtonText model =
    if (hasZeroScore model) then
        text "Play to share"
    else
        text "Share Score"


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
                , button [ onClick SaveName ] [ text "Save" ]
                , button [ onClick CancelName ] [ text "Cancel" ]
                ]

        Playing ->
            text ""


view : Model -> Html Msg
view model =
    div [ class "content" ]
        [ viewHeader "BUZZWORD BINGO"
        , viewPlayer model.name model.gameNumber
        , viewAlertMessage model.alertMessage
        , viewNameInput model
        , viewEntryList model.entries
        , viewScore (sumMarkedPoints model.entries)
        , div [ class "button-group" ]
            [ button [ onClick NewGame ] [ text "New Game" ]
            , button [ onClick Sort ] [ text "Sort" ]
            , button [ classList [ ( "disabled", hasZeroScore model ) ], onClick ShareScore, disabled (hasZeroScore model) ] [ viewScoreButtonText model ]
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
    Html.program
        { init = ( initialModel, getEntries )
        , view = view
        , update = update
        , subscriptions = (always Sub.none)
        }
