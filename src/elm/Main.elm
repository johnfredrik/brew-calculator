module Main exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing ( onInput, onClick,on )
import Round exposing( round )
import Components.Formula exposing(..)
import String
import Debug
import Json.Decode exposing (map)


-- APP
main : Program Never Model Msg
main =
  Html.beginnerProgram { model = model, view = view, update = update }


-- MODEL
type alias Model =
  { abv : Abv
  , ibu : Ibu
  , calculator : Calculator
  , error : String
  }

model : Model
model = 

    { abv = abv
    , ibu = ibu
    , calculator = IbuCalculator
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

type alias Ibu =
  { og : Float
  , boilVolume : Float
  , hops : List Hop
  , totalRager : Float
  , totalTinseth : Float
  }

ibu : Ibu
ibu = 
  { og = 1.054, boilVolume = 20, hops = [], totalRager = 0, totalTinseth = 0}
    

type alias Hop =
  { id : Int
  , name : String
  , aa : Float
  , amount : Float
  , boilTime : Int
  , rager : Float
  , tinseth : Float
  , hopType : String
  }

-- hops : List Hop
-- hops =
--   [ {id = 1, name = "Hop1", aa = 3.5, amount = 20, boilTime = 60, rager = 0, tinseth = 0}
--   , {id = 2, name = "Hop2", aa = 3.6, amount = 21, boilTime = 15, rager = 0, tinseth = 0}
--   ]

hop : Int -> Hop 
hop id =
  {id = id, name = "Hop2", aa = 0.0, amount = 0, boilTime = 0, rager = 0, tinseth = 0, hopType = "Whole"}

type Calculator
  = AbvCalculator
  | IbuCalculator

--   UPDATE
type Msg 
  = NoOp
  | SetOG String
  | SetFG String
  | ChangeCalculator Calculator
  | AddHop
  | RemoveHop Int
  | SetAlphaAcid Hop String
  | SetBoilTime Hop String
  | SetAmount Hop String
  | HopTypeSelected Hop String

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
    ChangeCalculator calculator ->
       { model | calculator = calculator}
    AddHop ->
      let
        hopId = (nextHopId model.ibu.hops)
        oldIbu = model.ibu
        newIbu = { oldIbu | hops = (oldIbu.hops ++ [(hop hopId) ])}
      in
        { model | ibu = newIbu}
    RemoveHop id ->
      let
        oldIbu = model.ibu
        hops = List.filter (\h -> h.id /= id) model.ibu.hops
                |> List.map (recalculateIbu ibu.boilVolume ibu.og)
        newIbu = { oldIbu | hops = hops, totalRager = calculateRager hops, totalTinseth = calculateTinseth hops}
      in
        { model | ibu = newIbu}
    SetAlphaAcid hop alphaAcid ->
      let
        updatedHop = 
          case (String.toFloat alphaAcid) of
            Ok number ->
              { hop | aa = number }
            Err error ->
              hop
        oldIbu = model.ibu
        hops = List.map (updateHop updatedHop) oldIbu.hops
                |> List.map (recalculateIbu ibu.boilVolume ibu.og)
        newIbu = { oldIbu | hops = hops, totalRager = calculateRager hops, totalTinseth = calculateTinseth hops}
      in
        {model | ibu = newIbu}
    SetBoilTime hop boilTime ->
      let
        updatedHop = 
          case (String.toInt boilTime) of
            Ok number ->
              {hop | boilTime = number}
            Err error ->
              hop
        oldIbu = model.ibu
        hops = List.map (updateHop updatedHop) oldIbu.hops
                |> List.map (recalculateIbu ibu.boilVolume ibu.og)
        newIbu = { oldIbu | hops = hops, totalRager = calculateRager hops, totalTinseth = calculateTinseth hops}
      in
        {model | ibu = newIbu}
    SetAmount hop amount ->
      let
        updatedHop = 
          case (String.toFloat amount) of
            Ok number ->
              {hop | amount = number}
            Err error ->
              hop
        oldIbu = model.ibu
        hops = List.map (updateHop updatedHop) oldIbu.hops
                |> List.map (recalculateIbu ibu.boilVolume ibu.og)
        newIbu = { oldIbu | hops = hops, totalRager = calculateRager hops, totalTinseth = calculateTinseth hops}
      in
        {model | ibu = newIbu}
    HopTypeSelected hop hopType ->
      let
        oldIbu = model.ibu
        updatedHop = 
          {hop | hopType = hopType}
        hops = List.map (updateHop updatedHop) oldIbu.hops
                |> List.map (recalculateIbu ibu.boilVolume ibu.og)
        newIbu = { oldIbu | hops = hops, totalRager = calculateRager hops, totalTinseth = calculateTinseth hops}
      in
        {model | ibu = newIbu}


-- VIEW
-- Html is defined as: elem [ attribs ][ children ]
-- CSS can be applied via class names or inline style attrib
view : Model -> Html Msg
view model =
  div 
    [ class "content"] 
    [ div [ class "header" ] 
          [ div [ classList [("selected-header", model.calculator == AbvCalculator)], onClick (ChangeCalculator AbvCalculator)] [ text "ABV" ]
          , div [ classList [("selected-header", model.calculator == IbuCalculator)], onClick (ChangeCalculator IbuCalculator)] [ text "IBU" ]
          ]
    , div []  
          [ div [ classList [("hide", model.calculator /= AbvCalculator)] ] [viewAbv model.abv]
          , div [ classList [("hide", model.calculator /= IbuCalculator)] ] [viewIbu model.ibu]
          ]
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

viewIbu : Ibu -> Html Msg
viewIbu ibu =
  div [] 
      [ h1 [] [text "IBU Calculator"]
      , div [] 
            [ label [] [ text "Boil Volume"]
            , input [ defaultValue (toString ibu.boilVolume) ] []
            ]
      , div []
            [ label [] [text "Original Gravity" ]
            , input [ defaultValue (toString ibu.og)] []
            ]
      , div [] 
            [ div [ class "ibu-title"] 
                  [ div [ class "ibu-column" ] [ text "Alfa Acids"] 
                  , div [ class "ibu-column" ] [ text "Amount"]
                  , div [ class "ibu-column" ] [ text "Boil Time"]
                  , div [ class "ibu-column" ] [ text "Hop Type" ]
                  , div [ class "ibu" ] [ text "Rager" ]
                  , div [ class "ibu" ] [ text "Tinseth" ]
                  ]
            , div [] (List.map viewHop ibu.hops)
            ]
      , div [] 
            [ button [ onClick AddHop ] [ text "Add hop"]
            , div [] [ text ("Rager Total: " ++ (Round.round 2 ibu.totalRager)) ]
            , div [] [ text ("Tinseth Total: " ++ (Round.round 2 ibu.totalTinseth)) ]
            ] 
            
      ]

viewHop : Hop -> Html Msg
viewHop hop =
  div [ class "hop"] 
      [ input [ class "ibu-column", defaultValue (toString hop.aa), onInput (SetAlphaAcid hop) ] []
      , input [ class "ibu-column", defaultValue (toString hop.amount), onInput (SetAmount hop) ] []
      , input [ class "ibu-column", defaultValue (toString hop.boilTime), onInput (SetBoilTime hop) ] []
      , viewHopType hop
      , div [ class "ibu"] [ text (Round.round 2 hop.rager) ]
      , div [ class "ibu"] [ text (Round.round 2 hop.tinseth) ]
      , button [ onClick (RemoveHop hop.id)] [ text "-"]
      ]

viewFormula : String -> Float -> Html Msg
viewFormula formula result =
  div [ class "formula" ] 
      [ div [ class "title" ] [ text formula]
      , div [ class "value" ] [ text ((Round.round 2 result) ++ "%")]
      ]

viewHopType : Hop -> Html Msg
viewHopType hop =
  div [ class "ibu-column"] 
      [ select [ onChange (HopTypeSelected hop)] 
        [ option [ selected (hop.hopType == "Whole")] [ text "Whole"]
        , option [ selected (hop.hopType == "Pellet") ] [ text "Pellet"]
        ]
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

recalculateIbu : Float -> Float -> Hop -> Hop
recalculateIbu boilVolume boilGravity hop =
  let
    factor = 
      if hop.hopType == "Pellet" then 
        1.1
      else 
        1.0
  in
    {hop 
      | rager = (rager (toFloat hop.boilTime) hop.amount hop.aa boilVolume boilGravity ) * factor
      , tinseth = (tinseth (toFloat hop.boilTime) hop.amount hop.aa boilVolume boilGravity ) * factor
    }

calculateRager : List Hop -> Float
calculateRager hops =
  List.map (\hop -> hop.rager) hops
    |> List.sum

calculateTinseth : List Hop -> Float
calculateTinseth hops =
  List.map (\hop -> hop.tinseth) hops
    |> List.sum

updateHop : Hop -> Hop -> Hop
updateHop newHop oldHop =
  if newHop.id == oldHop.id then
    { oldHop | aa = newHop.aa, boilTime = newHop.boilTime, amount = newHop.amount, hopType = newHop.hopType }
  else
    oldHop

nextHopId : List Hop -> Int
nextHopId hops = 
  let 
    ids = List.map (\hop -> hop.id) hops
  in
    case (List.maximum ids) of
      Nothing ->
        1
      Just number ->
        number + 1

onChange : (String -> msg) -> Html.Attribute msg
onChange tagger =
  on "change" (Json.Decode.map tagger Html.Events.targetValue)

