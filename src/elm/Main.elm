module Main exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing ( onInput, onClick,on )
import Round exposing( round )
import Components.Formula exposing(..)
import String
import Json.Decode exposing (string, int, list, map, float, Decoder)
import Http


-- APP
main : Program Never Model Msg
main =
  Html.program { init = (model, initialCmd), view = view, update = update, subscriptions = (\_ -> Sub.none) }


-- MODEL
type alias Model =
  { abv : Abv
  , ibu : Ibu
  , calculator : Calculator
  , error : Maybe String
  , hopComplete : HopComplete
  }

model : Model
model = 
    { abv = abv
    , ibu = ibu
    , calculator = IbuCalculator
    , error = Nothing
    , hopComplete = {hops = []}
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
  | LoadHops (Result Http.Error HopComplete)

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
    NoOp -> (model, Cmd.none)
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
       ({ model | abv = (recalculate newAbv) }, Cmd.none)
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
      ( { model | abv = recalculate newAbv}, Cmd.none)
    ChangeCalculator calculator ->
       ({ model | calculator = calculator}, Cmd.none)
    AddHop ->
      let
        hopId = (nextHopId model.ibu.hops)
        oldIbu = model.ibu
        newIbu = { oldIbu | hops = (oldIbu.hops ++ [(hop hopId) ])}
      in
        ({ model | ibu = newIbu}, Cmd.none)
    RemoveHop id ->
      let
        oldIbu = model.ibu
        hops = List.filter (\h -> h.id /= id) model.ibu.hops
                |> List.map (recalculateIbu ibu.boilVolume ibu.og)
        newIbu = { oldIbu | hops = hops, totalRager = calculateRager hops, totalTinseth = calculateTinseth hops}
      in
        ({ model | ibu = newIbu}, Cmd.none)
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
        ({model | ibu = newIbu}, Cmd.none)
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
        ({model | ibu = newIbu}, Cmd.none)
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
        ({model | ibu = newIbu}, Cmd.none)
    HopTypeSelected hop hopType ->
      let
        oldIbu = model.ibu
        updatedHop = 
          {hop | hopType = hopType}
        hops = List.map (updateHop updatedHop) oldIbu.hops
                |> List.map (recalculateIbu ibu.boilVolume ibu.og)
        newIbu = { oldIbu | hops = hops, totalRager = calculateRager hops, totalTinseth = calculateTinseth hops}
      in
        ({model | ibu = newIbu}, Cmd.none)
    LoadHops (Ok hopComplete) ->
        ({model | hopComplete = hopComplete}, Cmd.none)
    LoadHops (Err _) ->
      ({ model | error = Just "Loading hops failed"}, Cmd.none)


-- VIEW
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

viewIbu : Ibu -> Html Msg
viewIbu ibu =
  div [ class "ibu-calculator"] 
      [ h1 [] [text "IBU Calculator"]
      , div [ class "ibu-overhead"] 
            [ label [] [ text "Boil Volume"]
            , input [ defaultValue (toString ibu.boilVolume) ] []
            ]
      , div [ class "ibu-overhead"]
            [ label [] [text "Original Gravity" ]
            , input [ defaultValue (toString ibu.og)] []
            ]
      , div [] 
            [ div [ class "ibu-title"] 
                  [ h5 [ class "ibu-input" ] [ text "Alpha Acids"] 
                  , h5 [ class "ibu-input" ] [ text "Amount"]
                  , h5 [ class "ibu-input" ] [ text "Boil Time"]
                  , h5 [ class "ibu-select" ] [ text "Hop Type" ]
                  , h5 [ class "ibu-text" ] [ text "Rager" ]
                  , h5 [ class "ibu-text" ] [ text "Tinseth" ]
                  ]
            , div [ class "hops" ] (List.map viewHop ibu.hops)
            ]
      , div [ class "ibu-totals"] 
            [ div [] 
                [ div [ class "ibu-total"] 
                    [ h5 [] [ text "Rager Total:" ]
                    , h5 [class "ibu-total-value"] [ text (Round.round 2 ibu.totalRager)]
                    ]
                , div [ class "ibu-total"] 
                      [ h5 [] [ text "Tinseth Total:" ]
                      , h5 [ class "ibu-total-value"] [ text  (Round.round 2 ibu.totalTinseth)]
                      ]
                ]
            , button [ class "add-button", onClick AddHop ] [ text "Add hop"]
            ]
      ]

viewHop : Hop -> Html Msg
viewHop hop =
  div [ class "hop"]
      [ input [ class "ibu-input", defaultValue (toString hop.aa), onInput (SetAlphaAcid hop) ] []
      , input [ class "ibu-input", defaultValue (toString hop.amount), onInput (SetAmount hop) ] []
      , input [ class "ibu-input", defaultValue (toString hop.boilTime), onInput (SetBoilTime hop) ] []
      , select [ class "ibu-select", onChange (HopTypeSelected hop)] 
          [ option [ selected (hop.hopType == "Whole")] [ text "Whole"]
          , option [ selected (hop.hopType == "Pellet") ] [ text "Pellet"]
          ]
      , div [ class "ibu-text"] [ text (Round.round 2 hop.rager) ]
      , div [ class "ibu-text"] [ text (Round.round 2 hop.tinseth) ]
      , button [ class "remove-button", onClick (RemoveHop hop.id)] [ text "-"]
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



--HTTP Stuff

-- getHopString : String -> Request String
-- getHopString url =


type alias Hop2 =
  { id: Int
  , name : String
  , acid : Acid
  }

type alias Acid =
  { alpha : Alpha }

type alias Alpha =
  { low: Float, high: Float}


type alias HopComplete = 
  { hops: List Hop2 }

decodeHopComplete : Decoder HopComplete
decodeHopComplete = 
  Json.Decode.map 
    HopComplete
    (Json.Decode.field "hops" (Json.Decode.list decodeHop2))

decodeHop2 : Decoder Hop2
decodeHop2 =
  Json.Decode.map3
    Hop2
    (Json.Decode.field "hopId" int)
    (Json.Decode.field "name" string)
    (Json.Decode.field "acids" decodeAcid)

decodeAcid : Decoder Acid
decodeAcid =
  Json.Decode.map
    Acid
    (Json.Decode.field "alpha" decodeAlpha)

decodeAlpha : Decoder Alpha
decodeAlpha =
  Json.Decode.map2
    Alpha
    (Json.Decode.field "low" Json.Decode.float)
    (Json.Decode.field "high" Json.Decode.float)

initialCmd : Cmd Msg
initialCmd =
  decodeHopComplete
    |> Http.get "https://api.microbrew.it/hops?from=0&size=1000"
    |> Http.send LoadHops

