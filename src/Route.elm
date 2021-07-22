module Route exposing
    ( Route(..)
    , fromString
    , fromUrl
    , toName
    , toString
    )

import Url


type Route
    = StartPage
    | Logout
    | Counter
    | CP
    | Error404 String


fromUrl : Url.Url -> Route
fromUrl url =
    url.fragment |> Maybe.withDefault (toString StartPage) |> (++) "#" |> fromString



-- fromString <| "#" ++ ( )


fromString : String -> Route
fromString url =
    case String.toLower url of
        "" ->
            StartPage

        "#" ->
            StartPage

        "#cp" ->
            CP

        "#logout" ->
            Logout

        "#counter" ->
            Counter

        _ ->
            Error404 url


toString : Route -> String
toString route =
    case route of
        StartPage ->
            ""

        Logout ->
            "#logout"

        CP ->
            "#cp"

        Counter ->
            "#counter"

        Error404 url ->
            url


toName : Route -> String
toName route =
    case route of
        StartPage ->
            "Website NAME"

        Logout ->
            "Logout"

        CP ->
            "CP"

        Counter ->
            "Counter"

        Error404 url ->
            "404 Not Found Page"
