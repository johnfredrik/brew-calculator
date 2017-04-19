module Abv exposing(..)
import Formula exposing(..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing ( onInput, onClick,on )
import Round exposing( round )


type alias Model =
  { og : Float
  , fg : Float
  , miller : Float
  , simple : Float
  , alternativeSimple : Float
  , advanced : Float
  , alternativeAdvanced : Float
  , microbrewit : Float
  }

type Msg 
  = SetOG String
  | SetFG String


update : Msg -> Model -> Model
update msg model = 
  case msg of 
    SetOG ogString ->
      let
        og = String.toFloat ogString
        newAbv =
          case og of
          Ok number ->
            { model | og = number }
          Err error ->
             model 
      in
       (recalculate newAbv)
    SetFG fgString ->
      let
        fg = String.toFloat fgString
        newAbv = 
          case fg of
          Ok number ->
            { model | fg = number}
          Err error ->
            model
      in
      (recalculate newAbv)

init : Model
init = 
  let
    og = 1.054
    fg = 1.010
  in
    { og = og
    , fg = fg
    , miller = miller og fg
    , simple = simple og fg
    , alternativeSimple = alternativeSimple og fg 
    , advanced = advanced og fg
    , alternativeAdvanced = alternativeAdvanced og fg
    , microbrewit = microbrewit og fg
    }

recalculate : Model -> Model
recalculate abv =
  { abv 
    | miller = miller abv.og abv.fg
    , simple = simple abv.og abv.fg
    , alternativeSimple = alternativeSimple abv.og abv.fg 
    , advanced = advanced abv.og abv.fg
    , alternativeAdvanced = alternativeAdvanced abv.og abv.fg
    , microbrewit = microbrewit abv.og abv.fg
    }

viewAbv : Model -> Html Msg
viewAbv abv =
  div [ class "abv-calculator"]
      [ div [] 
          [ h1 [] [text "ABV Calculator"] ]
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
                [ div[ class "title" ] [text "Formula" ]
                , div[ class "value" ] [text "Value"]
                ]
          , viewFormula "Miller:" abv.miller
          , viewFormula "Simple:" abv.simple
          , viewFormula "Alternative Simple:" abv.alternativeSimple
          , viewFormula "Advanced:" abv.advanced
          , viewFormula "Alternative Advanced:" abv.alternativeAdvanced
          , viewFormula "Microbrewit:" abv.microbrewit
          ]
      ]

viewFormula : String -> Float -> Html Msg
viewFormula formula result =
  div [ class "formula" ] 
      [ div [ class "title" ] [ text formula]
      , div [ class "value" ] [ text ((Round.round 2 result) ++ "%")]
      ]