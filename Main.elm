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
    , gradeDesired : String
    , mdl : Material.Model
    }


model : Model
model =
    { numComponents = 1
    , components = empty
    , gradeToPass = 0
    , gradeDesired = "C"
    , mdl = Material.model
    }



--UPDATE


type Msg
    = ComponentInc
    | ComponentDec
    | ComponentWeight String String
    | ComponentPercentage String String
    | GradeChange String
    | Submit
    | Mdl (Material.Msg Msg)


convertRawFloat : String -> Float
convertRawFloat str =
    Result.withDefault 0 (String.toFloat <| str)


convertRawInt : String -> Int
convertRawInt str =
    Result.withDefault 0 (String.toInt <| str)


percentageFromLetter : String -> Int
percentageFromLetter letter =
    case letter of
        "A" ->
            85

        "A-" ->
            80

        "B+" ->
            75

        "B" ->
            70

        "B-" ->
            65

        "C+" ->
            60

        "C" ->
            55

        _ ->
            0


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
                    letterPercentage =
                        percentageFromLetter model_.gradeDesired
                in
                    let
                        pointsToPass =
                            letterPercentage - totalWithoutFinal
                    in
                        round ((toFloat pointsToPass / toFloat finalWeight) * 100)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model_ =
    case msg of
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

        GradeChange grade ->
            ( { model_ | gradeDesired = grade }, Cmd.none )

        Submit ->
            ( { model_ | gradeToPass = calculate model_ }, Cmd.none )

        Mdl msg_ ->
            Material.update msg_ model_



--VIEW


type alias Mdl =
    Material.Model


view : Model -> Html Msg
view model_ =
    div [ class "main-div " ]
        [ h1 [] [ text "Grade to Pass" ]
        , div
            [ class "buttons-div" ]
            [ Button.render Mdl
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
            , renderComponents model_
            , Textfield.render Mdl [ 3 ] model_.mdl [ Textfield.onInput GradeChange, Textfield.label "Grade desired" ]
            , Button.render Mdl
                [ 2 ]
                model_.mdl
                [ Button.onClick Submit ]
                [ text "Calculate" ]
            , text (toString <| model_.gradeToPass)
            ]
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
                    div [ class "wrapper" ]
                        [ Textfield.render Mdl [ 0 ] model_.mdl [ Textfield.onInput (ComponentWeight (toString <| (first tuple))), Textfield.label "Component" ], Textfield.render Mdl [ 1 ] model_.mdl [ Textfield.onInput (ComponentPercentage (toString <| (first tuple))), Textfield.label "Percentage" ] ]
            in
                let
                    inputsWithIds =
                        List.map addIds tupledList
                in
                    div [ class "input-div" ] inputsWithIds
