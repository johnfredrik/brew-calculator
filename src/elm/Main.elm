module Main exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing ( onInput )
import Round exposing( round )
import String



-- APP
main : Program Never Model Msg
main =
  Html.beginnerProgram { model = model, view = view, update = update }


-- MODEL
type alias Model =
  { og : Float
  , fg : Float
  , miller : Float
  , simple : Float
  , error : String
  }

model : Model
model = 
  let
    og = 1.054
    fg = 1.010
  in
    {og = og, fg = fg, miller = (miller og fg), simple = (simple og fg), error = ""}

type Formula
  = Miller
  | Simple



-- UPDATE
type Msg 
  = NoOp
  | SetOG String
  | SetFG String

update : Msg -> Model -> Model
update msg model =
  case msg of
    NoOp -> model
    SetOG ogString ->
      let
        og = String.toFloat ogString
        newModel =
          case og of
          Ok number ->
            { model | og = number }
          Err error ->
            {model | error = error } 
      in
        recalculate newModel
    SetFG fgString ->
      let
        fg = String.toFloat fgString
        newModel = 
          case fg of
          Ok number ->
            { model | fg = number}
          Err error ->
            {model | error = error }
      in
        recalculate newModel


-- VIEW
-- Html is defined as: elem [ attribs ][ children ]
-- CSS can be applied via class names or inline style attrib
view : Model -> Html Msg
view model =
  div 
    [ class "content"] 
    [ div [ class "header" ] 
          [ h1 [] [text "ABV calculator"] ]
    , div [class "input-groups"] 
          [ div [ class "inputs"]
                [ label [] [ text "OG:" ]
                  , input [ type_ "number", defaultValue "1.054", onInput SetOG] [ ]
                ]
          , div [ class "inputs"]
                [ label [] [ text "FG:" ]
                , input [ type_ "number", defaultValue "1.010", onInput SetFG] [ ]
                ]
          ]
    , div [ class "formulas"]
          [ div [ class "formula" ] 
                [ div[] [text "Formulas:" ]
                , div[] [text "Value:"]
                ]
          , viewFormula "Miller:" model.miller
          , viewFormula "Simple:" model.simple
          ]
    ]

viewFormula : String -> Float -> Html Msg
viewFormula formula result =
  div [ class "formula" ] 
      [ div [] [ text formula]
      , div [] [ text ((Round.round 2 result) ++ "%")]
      ]


recalculate : Model -> Model
recalculate model =
  { model | miller = (miller model.og model.fg), simple = (simple model.og model.fg)}


miller : Float -> Float -> Float
miller og fg =
    (og - fg) / 0.75 * 100

simple : Float -> Float -> Float
simple og fg =
  (og - fg) * 131.25


      