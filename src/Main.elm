module Main exposing (..)

import Array exposing (Array)
import Browser
import Html exposing (Html, div, text)
import Html.Attributes exposing (class, href, style)
import Material
import Material.Button as Button
import Material.Options as Options exposing (css, styled)



-- import Material.Helpers as MH -- exposing (pure)


type alias Model =
    { mdc : Material.Model Msg
    , counters : Array Int
    }


defaultModel : Model
defaultModel =
    { mdc = Material.defaultModel
    , counters = Array.fromList [ 1, 3, 0 ] -- Array.empty
    }


type Msg
    = Mdc (Material.Msg Msg)
    | Click
      -- Counters
    | Increase Int
    | Reset Int
    | Add
    | Remove


map : Int -> (a -> a) -> Array a -> Array a
map k f a =
    Array.get k a
        |> Maybe.map (\x -> Array.set k (f (Debug.log "" x)) a)
        |> Maybe.withDefault a


map2 : Int -> (a -> a) -> Array a -> Array a
map2 k f a =
    Maybe.withDefault a (Maybe.map (\x -> Array.set k (f (Debug.log "" x)) a) (Array.get k a))


main : Program () Model Msg
main =
    Browser.element
        { init = \_ -> init
        , subscriptions = subscriptions
        , update = update
        , view = view
        }


init : ( Model, Cmd Msg )
init =
    ( defaultModel, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    Material.subscriptions Mdc model


arrayPop : Array a -> Array a
arrayPop =
    Array.slice 0 -1


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Mdc msg_ ->
            Material.update Mdc msg_ model

        Click ->
            ( model, Cmd.none )

        Increase k ->
            ( { model | counters = map k ((+) 1) model.counters }, Cmd.none )

        Reset k ->
            ( { model | counters = map k (always 0) model.counters }, Cmd.none )

        Add ->
            ( { model | counters = Array.push 0 model.counters }, Cmd.none )

        Remove ->
            ( { model | counters = arrayPop model.counters }, Cmd.none )


view1 : Model -> Int -> Int -> Html Msg
view1 model idx val =
    div
        []
        -- [ style [("padding" ,"2em")]]
        [ text ("Current count: " ++ String.fromInt val)
        , button ("reset-" ++ String.fromInt idx) (Reset idx) "Reset" model []
        , button ("incr-" ++ String.fromInt idx) (Increase idx) "Inc" model []
        ]


view : Model -> Html Msg
view model =
    let
        counters =
            model.counters |> Array.toList |> List.indexedMap (view1 model)
    in
    styled Html.div
        [ css "padding" "1em" ]
        [ button "my-button" Click "Click me!" model []
        , div
            []
            (List.concat
                [ [ button "add-counter" Add "Add Counter!" model [] ]
                , [ button "rem-counter"
                        Remove
                        "Remove Counter!"
                        model
                        (if List.length counters == 0 then
                            [ Button.disabled ]

                         else
                            []
                        )
                  ]
                , counters
                ]
            )
        ]


button : String -> Msg -> String -> Model -> List (Button.Property Msg) -> Html Msg
button uniqId msg label model maybeAttrs =
    Button.view Mdc
        uniqId
        model.mdc
        (List.concat
            [ [ Button.ripple
              , Button.raised
              , Options.onClick msg
              ]
            , maybeAttrs
            ]
        )
        [ text label ]
