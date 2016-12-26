module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Array exposing (..)
import Tuple exposing (..)
import Material
import Material.Scheme
import Material.Button as Button
import Material.Textfield as Textfield
import Material.Options exposing (css)


main : Program Never Model Msg
main =
    Html.program
        { init = ( model, Cmd.none )
        , view = view
        , subscriptions = always Sub.none
        , update = update
        }



--MODEL


type alias Percentages =
    { weight : Float
    , percentage : Float
    }


type alias Model =
    { numComponents : Int
    , components : Array Percentages
    , gradeToPass : Int
    , mdl : Material.Model
    }


model : Model
model =
    { numComponents = 0
    , components = empty
    , gradeToPass = 0
    , mdl = Material.model
    }



--UPDATE


type Msg
    = ComponentInc
    | ComponentDec
    | ComponentWeight String String
    | ComponentPercentage String String
    | Submit
    | Mdl (Material.Msg Msg)


convertRawFloat : String -> Float
convertRawFloat str =
    Result.withDefault 0 (String.toFloat <| str)


convertRawInt : String -> Int
convertRawInt str =
    Result.withDefault 0 (String.toInt <| str)


calculate : Model -> Int
calculate model_ =
    let
        withoutFinal : Int -> Int -> Int
        withoutFinal total index =
            if index > model_.numComponents - 1 then
                total
            else
                let
                    currentComponent =
                        Maybe.withDefault { weight = 0, percentage = 0 } (get index model_.components)
                in
                    let
                        newTotal =
                            total + (round <| (currentComponent.weight / 100) * currentComponent.percentage)
                    in
                        withoutFinal newTotal (index + 1)

        computeFinalWeight : Int -> Int -> Int
        computeFinalWeight total index =
            if index > model_.numComponents - 1 then
                total
            else
                let
                    currentComponent =
                        Maybe.withDefault { weight = 0, percentage = 0 } (get index model_.components)
                in
                    let
                        newTotal =
                            total + (round <| (currentComponent.weight))
                    in
                        100 - computeFinalWeight newTotal (index + 1)
    in
        let
            totalWithoutFinal =
                withoutFinal 0 0
        in
            let
                finalWeight =
                    computeFinalWeight 0 0
            in
                let
                    pointsToPass =
                        55 - totalWithoutFinal
                in
                    round ((toFloat pointsToPass / toFloat finalWeight) * 100)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model_ =
    case msg of
        -- increment numComponents integer and add empty record to components array
        ComponentInc ->
            ( { model_ | numComponents = model_.numComponents + 1, components = push { weight = 0, percentage = 0 } model_.components }, Cmd.none )

        ComponentDec ->
            ( { model_ | numComponents = model_.numComponents - 1, components = slice 0 (model_.numComponents - 1) model_.components }, Cmd.none )

        ComponentWeight id_ weight ->
            let
                record =
                    Maybe.withDefault { weight = 0, percentage = 0 } (get (convertRawInt id_) model_.components)
            in
                let
                    updatedRecord =
                        { record | weight = convertRawFloat weight }
                in
                    ( { model_ | components = set (convertRawInt id_) updatedRecord model_.components }, Cmd.none )

        ComponentPercentage id_ percentage ->
            let
                record =
                    Maybe.withDefault { weight = 0, percentage = 0 } (get (convertRawInt id_) model_.components)
            in
                let
                    updatedRecord =
                        { record | percentage = convertRawFloat percentage }
                in
                    ( { model_ | components = set (convertRawInt id_) updatedRecord model_.components }, Cmd.none )

        Submit ->
            ( { model_ | gradeToPass = calculate model_ }, Cmd.none )

        Mdl msg_ ->
            Material.update msg_ model_



--VIEW


type alias Mdl =
    Material.Model


view : Model -> Html Msg
view model_ =
    div []
        [ renderComponents model_
        , Button.render Mdl
            [ 0 ]
            model_.mdl
            [ Button.onClick ComponentInc
            , css "margin" "0 24px"
            ]
            [ text "Add Component" ]
        , Button.render Mdl
            [ 1 ]
            model_.mdl
            [ Button.onClick ComponentDec ]
            [ text "Remove Component" ]
        , Button.render Mdl
            [ 2 ]
            model_.mdl
            [ Button.onClick Submit ]
            [ text "Calculate" ]
        , text (toString <| model_.gradeToPass)
        ]
        |> Material.Scheme.top


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
                    div []
                        [ Textfield.render Mdl [ 0 ] model_.mdl [ Textfield.onInput (ComponentWeight (toString <| (first tuple))) ], Textfield.render Mdl [ 0 ] model_.mdl [ Textfield.onInput (ComponentPercentage (toString <| (first tuple))) ] ]

                -- div
                -- []
                -- [ input [ type_ "number", onInput (ComponentWeight (toString <| (first tuple))) ] [], input [ type_ "number", onInput (ComponentPercentage (toString <| (first tuple))) ] [] ]
            in
                let
                    inputsWithIds =
                        List.map addIds tupledList
                in
                    div [] inputsWithIds
