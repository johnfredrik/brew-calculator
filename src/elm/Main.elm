module Main exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing ( onInput )
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
      in
        case og of
          Ok number ->
            { model | og = number}
          Err error ->
            {model | error = error }
    SetFG fgString ->
      let
        fg = String.toFloat fgString
      in
        case fg of
          Ok number ->
            { model | og = number}
          Err error ->
            {model | error = error }


-- VIEW
-- Html is defined as: elem [ attribs ][ children ]
-- CSS can be applied via class names or inline style attrib
view : Model -> Html Msg
view model =
  div 
    [] 
    [ h1 [] [text "ABV calculator"]
    , h3 [] [ text model.error]
    , div [] 
      [ label [] [ text "OG" ]
      , input [ type_ "text", value (toString model.og), onInput SetOG] [ ]
      , label [] [ text "FG" ]
      , input [ type_ "text", value (toString model.fg), onInput SetFG] [ ]
      ]
    , viewFormula "Miller:" model.miller
    , viewFormula "Simple:" model.simple
    
    ]

viewFormula : String -> Float -> Html Msg
viewFormula formula result =
  div [] 
      [ span [] [ text formula]
      , span [] [ text (toString result)]
      ]


miller : Float -> Float -> Float
miller og fg =
    (og - fg) / 0.75 * 100

simple : Float -> Float -> Float
simple og fg =
  (og - fg) * 131.25
