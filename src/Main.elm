module Main exposing (..)

import Array exposing (Array)
import Browser
import Html exposing (Html, div, h1, h3, p, text)
import Html.Attributes exposing (class, href, style)
import Internal.Menu.Model exposing (Msg(..))
import Internal.Tooltip.Model exposing (YTransformOrigin(..))
import Material
import Material.Button as Button
import Material.Drawer.Modal as Drawer
import Material.List as Lists
import Material.Options as Options exposing (css, styled, when)
import Material.TopAppBar as TopAppBar



-- import Material.Helpers as MH -- exposing (pure)


type alias Model =
    { mdc : Material.Model Msg
    , counters : Array Int
    , drawer_open : Bool
    }


defaultModel : Model
defaultModel =
    { mdc = Material.defaultModel
    , counters = Array.fromList [ 1, 3, 0 ] -- Array.empty
    , drawer_open = False
    }


type Msg
    = Mdc (Material.Msg Msg)
    | Click
      -- Counters
    | Increase Int
    | Reset Int
    | Add
    | Remove
      -- Drawer
    | OpenDrawer
    | CloseDrawer


map : Int -> (a -> a) -> Array a -> Array a
map k f a =
    Array.get k a
        |> Maybe.map (\x -> Array.set k (f (Debug.log "" x)) a)
        |> Maybe.withDefault a


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

        -- Counters
        Increase k ->
            ( { model | counters = map k ((+) 1) model.counters }, Cmd.none )

        Reset k ->
            ( { model | counters = map k (always 0) model.counters }, Cmd.none )

        Add ->
            ( { model | counters = Array.push 0 model.counters }, Cmd.none )

        Remove ->
            ( { model | counters = arrayPop model.counters }, Cmd.none )

        -- Drawer
        OpenDrawer ->
            ( { model | drawer_open = True }, Cmd.none )

        CloseDrawer ->
            ( { model | drawer_open = False }, Cmd.none )


view1 : Model -> Int -> Int -> Html Msg
view1 model idx val =
    div
        []
        -- [ style [("padding" ,"2em")]]
        [ text ("Current count: " ++ String.fromInt val)
        , button ("reset-" ++ String.fromInt idx) (Reset idx) "Reset" model []
        , button ("incr-" ++ String.fromInt idx) (Increase idx) "Inc" model []
        ]


viewTopAppBar : Model -> Html Msg
viewTopAppBar model =
    TopAppBar.view Mdc
        "my-top-app-bar"
        model.mdc
        []
        [ TopAppBar.section
            [ TopAppBar.alignStart
            ]
            [ TopAppBar.navigationIcon Mdc
                "my-top-app-bar--menu"
                model.mdc
                [ Options.onClick OpenDrawer ]
                "menu"
            , TopAppBar.title [] [ text "Basic App Example" ]
            ]
        ]


viewDrawer : Model -> Html Msg
viewDrawer model =
    Drawer.view Mdc
        "my-drawer"
        model.mdc
        [ Drawer.open |> when model.drawer_open
        , Drawer.onClose CloseDrawer
        ]
        [ Drawer.header
            []
            [ styled h3 [ Drawer.title ] [ text "A Header" ]
            ]
        , Drawer.content []
            [ Lists.nav Mdc
                "my-drawer-list"
                model.mdc
                []
                [ drawerLink "Dashboard"
                , drawerLink "My account"
                , Lists.hr [] []
                , drawerLink "Logout"
                ]
            ]
        ]


drawerLink : String -> Lists.ListItem Msg
drawerLink linkContent =
    Lists.a
        [ Options.attribute (href "#")
        , Lists.activated |> when isActive
        ]
        [ text linkContent ]


isActive =
    False


viewContent : Html Msg
viewContent =
    div []
        [ h1 [] [ text "My Content" ]
        , p [] [ text "BODY TEXT HERE" ]
        ]


view : Model -> Html Msg
view model =
    Html.div []
        [ viewTopAppBar model
        , viewDrawer model
        , Drawer.scrim [ Options.onClick CloseDrawer ] []
        , styled Html.main_ [ TopAppBar.fixedAdjust ] [ viewContent ]
        ]


counterView : Model -> Html Msg
counterView model =
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
