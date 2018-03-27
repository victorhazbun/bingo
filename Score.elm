module Score exposing (Score, viewScore, hasZeroScore, viewScoreButtonText, postScore)

import Html exposing (..)
import Html.Attributes exposing (..)
import Entry as BingoEntry
import Http
import Json.Decode.Pipeline as JsonDecodePipeline exposing (decode, required, optional, hardcoded)
import Json.Decode as Decode exposing (int, string, float, Decoder)
import Json.Encode as Encode


type alias Score =
    { id : Int
    , name : String
    , score : Int
    }


hasZeroScore : List BingoEntry.Entry -> Bool
hasZeroScore entries =
    (BingoEntry.sumMarkedPoints entries) == 0


viewScore : Int -> Html msg
viewScore sum =
    div [ class "score" ]
        [ span [ class "label" ] [ text "Score" ]
        , span [ class "value" ] [ text (toString sum) ]
        ]


viewScoreButtonText : List BingoEntry.Entry -> String
viewScoreButtonText entries =
    if (hasZeroScore entries) then
        "Play to share"
    else
        "Share Score"


postScore : (Result Http.Error Score -> msg) -> String -> String -> Int -> Cmd msg
postScore msg url name score =
    let
        body =
            encodeScore name score
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
        Http.send msg request



-- PRIVATE HELPERS --


scoreDecoder : Decoder Score
scoreDecoder =
    JsonDecodePipeline.decode Score
        |> JsonDecodePipeline.required "id" int
        |> JsonDecodePipeline.required "name" string
        |> JsonDecodePipeline.required "score" int


encodeScore : String -> Int -> Encode.Value
encodeScore name score =
    Encode.object
        [ ( "name", Encode.string name )
        , ( "score", Encode.int score )
        ]
