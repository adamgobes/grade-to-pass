module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Array exposing (..)
import Tuple exposing (..)


main =
    Html.beginnerProgram { model = model, view = view, update = update }



--MODEL


type alias Percentages =
    { weight : Int
    , percentage : Int
    }


type alias Model =
    { numComponents : Int
    , components : Array Percentages
    }


model : Model
model =
    Model 0 empty



--UPDATE


type Msg
    = ComponentInc
    | ComponentDec



-- | ComponentWeight String Int
-- | ComponentPercentage String Int


update : Msg -> Model -> Model
update msg model_ =
    case msg of
        -- increment numComponents integer and add empty record to components array
        ComponentInc ->
            { model_ | numComponents = model_.numComponents + 1, components = append (fromList [ { weight = 0, percentage = 0 } ]) model_.components }

        ComponentDec ->
            { model_ | numComponents = model_.numComponents - 1, components = slice 0 (model_.numComponents - 1) model_.components }



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
    -- create list of tuples [(index, list html msg)]
    let
        inputs =
            List.repeat model_.numComponents ([ input [ type_ "number" ] [], input [ type_ "number" ] [] ])
    in
        let
            tupledList =
                List.indexedMap (,) inputs
        in
            let
                addIds tuple =
                    div [] [ input [ type_ "number" ] [], input [ type_ "number" ] [] ]
            in
                let
                    inputsWithIds =
                        List.map addIds tupledList
                in
                    div [] inputsWithIds
