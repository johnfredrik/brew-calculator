module Main exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing ( onInput )
import Round exposing( round )
import Components.Formula exposing(..)
import String



-- APP
main : Program Never Model Msg
main =
  Html.beginnerProgram { model = model, view = view, update = update }


-- MODEL
type alias Model =
  { abv : Abv
  , error : String
  }

model : Model
model = 

    { abv = abv
    , error = ""
    }

type alias Abv =
  { og : Float
  , fg : Float
  , miller : Float
  , simple : Float
  , alternativeSimple : Float
  , advanced : Float
  , alternativeAdvanced : Float
  , microbrewit : Float
  }

abv : Abv
abv = 
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
--   UPDATE
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
        oldAbv = model.abv
        newAbv =
          case og of
          Ok number ->
            { oldAbv | og = number }
          Err error ->
            oldAbv 
      in
       { model | abv = (recalculate newAbv) }
    SetFG fgString ->
      let
        fg = String.toFloat fgString
        oldAbv = model.abv
        newAbv = 
          case fg of
          Ok number ->
            { oldAbv | fg = number}
          Err error ->
            oldAbv
      in
       { model | abv = recalculate newAbv}


-- VIEW
-- Html is defined as: elem [ attribs ][ children ]
-- CSS can be applied via class names or inline style attrib
view : Model -> Html Msg
view model =
  div 
    [ class "content"] 
    [ div [ class "header" ] 
          [ div [] [ text "ABV" ]
          , div [] [ text "IBU" ]
          ]
    , viewAbv model.abv
    ]

viewAbv : Abv -> Html Msg
viewAbv abv =
  div []
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


recalculate : Abv -> Abv
recalculate abv =
  { abv 
    | miller = miller abv.og abv.fg
    , simple = simple abv.og abv.fg
    , alternativeSimple = alternativeSimple abv.og abv.fg 
    , advanced = advanced abv.og abv.fg
    , alternativeAdvanced = alternativeAdvanced abv.og abv.fg
    , microbrewit = microbrewit abv.og abv.fg
    }
