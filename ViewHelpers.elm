module ViewHelpers exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)


viewHeader : String -> Html msg
viewHeader title =
    header []
        [ h1 [] [ text title ] ]


viewFooter : Html msg
viewFooter =
    footer []
        [ a [ href "http://elm-lang.org" ]
            [ text "Powered By Elm" ]
        ]


primaryButton : msg -> String -> Maybe Bool -> Maybe (List ( String, Bool )) -> Html msg
primaryButton msg txt isDisabled klassList =
    button
        [ classList (Maybe.withDefault [ ( "", False ) ] klassList)
        , class "primary"
        , onClick msg
        , disabled (Maybe.withDefault False isDisabled)
        ]
        [ text txt ]


alert : msg -> Maybe String -> Html msg
alert msg alertMessage =
    case alertMessage of
        Just message ->
            div [ class "alert" ]
                [ span [ class "close", onClick msg ] [ text "X" ]
                , text message
                ]

        Nothing ->
            text ""
