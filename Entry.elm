module Entry exposing (Entry, markEntryWithId, sumMarkedPoints, viewEntryList, getEntries)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Http
import Json.Decode as Decode exposing (int, string, float, Decoder)
import Json.Decode.Pipeline as JsonDecodePipeline exposing (decode, required, optional, hardcoded)


type alias Entry =
    { id : Int
    , phrase : String
    , points : Int
    , marked : Bool
    }


markEntryWithId : Int -> List Entry -> List Entry
markEntryWithId id entries =
    let
        markEntry e =
            if e.id == id then
                { e | marked = (not e.marked) }
            else
                e
    in
        entries
            |> List.map markEntry


sumMarkedPoints : List Entry -> Int
sumMarkedPoints entries =
    entries
        |> List.filter .marked
        |> List.foldl (\e sum -> sum + e.points) 0



-- DECODERS/ENCODERS


entryDecoder : Decoder Entry
entryDecoder =
    JsonDecodePipeline.decode Entry
        |> JsonDecodePipeline.required "id" int
        |> JsonDecodePipeline.required "phrase" string
        |> JsonDecodePipeline.optional "points" int 100
        |> JsonDecodePipeline.hardcoded False



-- COMMANDS


getEntries : (Result Http.Error (List Entry) -> msg) -> String -> Cmd msg
getEntries msg url =
    (Decode.list entryDecoder)
        |> Http.get url
        |> Http.send msg



-- VIEW


viewEntryItem : (Int -> msg) -> Entry -> Html msg
viewEntryItem msg entry =
    li [ classList [ ( "marked", entry.marked ) ], onClick (msg entry.id) ]
        [ span [ class "phrase" ] [ text entry.phrase ]
        , span [ class "points" ] [ text (toString entry.points) ]
        ]


viewEntryList : (Int -> msg) -> List Entry -> Html msg
viewEntryList msg entries =
    entries
        |> List.map (viewEntryItem msg)
        |> ul []
