module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)


main =
    Html.beginnerProgram { model = model, view = view, update = update }



--MODEL


type alias Percentages =
    ( Int, Int )


type alias Model =
    { components : Int
    }


model : Model
model =
    Model 0



--UPDATE


type Msg
    = ComponentInc
    | ComponentDec


update : Msg -> Model -> Model
update msg model_ =
    case msg of
        ComponentInc ->
            { model_ | components = model_.components + 1 }

        ComponentDec ->
            { model_ | components = model_.components - 1 }



--VIEW


view : Model -> Html Msg
view model_ =
    div []
        [ renderComponents model_
        , button [ onClick ComponentInc ] [ text "increment" ]
        , button [ onClick ComponentDec ] [ text "decrement" ]
        ]


renderComponents : Model -> Html Msg
renderComponents model_ =
    let
        inputs =
            List.repeat model_.components (div [] [ input [ type_ "number" ] [], input [ type_ "number" ] [] ])
    in
        div [] inputs
