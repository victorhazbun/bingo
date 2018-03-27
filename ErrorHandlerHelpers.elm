module ErrorHandlerHelpers exposing (httpErrorToMessage)

import Http


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
