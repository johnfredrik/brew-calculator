module Main exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing ( onInput, onClick,on )
import Round exposing( round )
import Components.Formula exposing(..)
import Components.Hop as Hop exposing(..)
import String
import Json.Decode exposing (map)
import Http


-- APP
main : Program Never Model Msg
main =
  Html.program { init = (model, initialCmd), view = view, update = update, subscriptions = (\_ -> Sub.none) }


-- MODEL
type alias Model =
  { abv : Abv
  , ibu : Ibu
  , srm : Srm
  , calculator : Calculator
  , error : Maybe String
  }

model : Model
model = 
    { abv = abv
    , ibu = ibu
    , srm = initSrm
    , calculator = SrmCalculator
    , error = Nothing
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
  , hopList : List Hop
  }

ibu : Ibu
ibu = 
  { og = 1.054, boilVolume = 20, hops = [], totalRager = 0, totalTinseth = 0, hopList = []}


type alias Srm =
  { daniels: Float
  , fermentables: List Fermentable
  , morey : Float 
  , mosher: Float
  , volume: Float
  }

initSrm : Srm
initSrm =
  {daniels = 0.0, fermentables = [], morey = 0.0, mosher = 0.0, volume = 0.0}

type Calculator
  = AbvCalculator
  | IbuCalculator
  | SrmCalculator

--   UPDATE
type Msg 
  = NoOp
  | SetOG String
  | SetFG String
  | ChangeCalculator Calculator
  | AddHop
  | RemoveHop Int
  | SetHopName Hop String
  | SetHopAlphaAcid Hop String
  | SetHopBoilTime Hop String
  | SetHopAmount Hop String
  | HopTypeSelected Hop String
  | LoadHops (Result Http.Error HopComplete)
  | SearchHops String
  | AddMbHop Hop
  | SetFermentableName Fermentable String
  | SetFermentableLovibond Fermentable String
  | SetFermentableAmount Fermentable String
  | AddFermentable

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
        hopId = (nextIndex model.ibu.hops)
        oldIbu = model.ibu
        newIbu = { oldIbu | hops = (oldIbu.hops ++ [(Hop.init hopId) ])}
      in
        ({ model | ibu = newIbu}, Cmd.none)
    RemoveHop index ->
      let
        oldIbu = model.ibu
        hops = List.filter (\h -> h.index /= index) model.ibu.hops
                |> List.map (recalculateIbu ibu.boilVolume ibu.og)
        newIbu = { oldIbu | hops = hops, totalRager = calculateRager hops, totalTinseth = calculateTinseth hops}
      in
        ({ model | ibu = newIbu}, Cmd.none)
    SetHopName hop name ->
      let
        updatedHop = {hop | name = name}
        oldIbu = model.ibu
        hops = List.map (updateHop updatedHop) oldIbu.hops
        newIbu = { oldIbu | hops = hops}
      in
        ({model | ibu = newIbu}, Cmd.none)
    SetHopAlphaAcid hop alphaAcid ->
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
    SetHopBoilTime hop boilTime ->
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
    SetHopAmount hop amount ->
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
      let
        oldIbu = model.ibu
        newIbu = { oldIbu | hopList = hopComplete.hops}
      in
        ({model | ibu = newIbu}, Cmd.none)
    LoadHops (Err _) ->
      ({ model | error = Just "Loading hops failed"}, Cmd.none)
    SearchHops query ->
      if (String.length query) > 2 then
        (model, searchHops query)
      else if (String.length query) == 0 then
        (model, initialCmd)
      else 
        (model, Cmd.none)
    AddMbHop hop ->
      let
        newHop = { index = (nextIndex model.ibu.hops),id = hop.id, name = hop.name, aa = hop.acid.alpha.low, amount = 0, boilTime = 0, rager = 0, tinseth = 0, hopType = "Whole", acid = initAcid}
        oldIbu = model.ibu
        newIbu = { oldIbu | hops = (oldIbu.hops ++ [newHop])}
      in
        ({model | ibu = newIbu}, Cmd.none)
    SetFermentableName fermentable name ->
      (model, Cmd.none)
    SetFermentableLovibond fermentable lovibond ->
      (model, Cmd.none)
    SetFermentableAmount fermentalbe lovibond ->
      (model, Cmd.none)
    AddFermentable ->
      let
        fermentable = initFermentable (nextIndex model.srm.fermentables)
        oldSrm = model.srm
        newSrm = { oldSrm | fermentables = (oldSrm.fermentables ++ [fermentable]) }
      in
        ({model | srm = newSrm}, Cmd.none)
    


-- VIEW
view : Model -> Html Msg
view model =
  div 
    [ class "content"] 
    [ div [ class "header" ] 
          [ div [ classList [("selected-header", model.calculator == AbvCalculator)], onClick (ChangeCalculator AbvCalculator)] [ text "ABV" ]
          , div [ classList [("selected-header", model.calculator == IbuCalculator)], onClick (ChangeCalculator IbuCalculator)] [ text "IBU" ]
          , div [ classList [("selected-header", model.calculator == SrmCalculator)], onClick (ChangeCalculator SrmCalculator)] [ text "SRM" ]
          ]
    , div []  
          [ div [ classList [("hide", model.calculator /= AbvCalculator)] ] [viewAbv model.abv]
          , div [ classList [("hide", model.calculator /= IbuCalculator)] ] [viewIbu model.ibu]
          , div [ classList [("hide", model.calculator /= SrmCalculator)] ] [viewSrm model.srm]
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
                  [ h5 [ class "ibu-input" ] [ text "Name"] 
                  , h5 [ class "ibu-input" ] [ text "Alpha Acids"] 
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
            , button [ class "add-button", onClick AddHop ] [ text "add new"]
            ]
      , viewHopList ibu.hopList
      ]

viewHop : Hop -> Html Msg
viewHop hop =
  div [ class "hop"]
      [ input [ class "ibu-input", defaultValue hop.name, onInput (SetHopName hop)] []
      , input [ class "ibu-input", defaultValue (toString hop.aa), onInput (SetHopAlphaAcid hop) ] []
      , input [ class "ibu-input", defaultValue (toString hop.amount), onInput (SetHopAmount hop) ] []
      , input [ class "ibu-input", defaultValue (toString hop.boilTime), onInput (SetHopBoilTime hop) ] []
      , select [ class "ibu-select", onChange (HopTypeSelected hop)] 
          [ option [ selected (hop.hopType == "Whole")] [ text "Whole"]
          , option [ selected (hop.hopType == "Pellet") ] [ text "Pellet"]
          ]
      , div [ class "ibu-text"] [ text (Round.round 2 hop.rager) ]
      , div [ class "ibu-text"] [ text (Round.round 2 hop.tinseth) ]
      , button [ class "remove-button", onClick (RemoveHop hop.index)] [ text "-"]
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

viewHopList : List Hop -> Html Msg
viewHopList hops =
  div [class "mb-hops"]
    [ input [ class "mb-search", placeholder "search hop", onInput SearchHops] []
    , div [ class "mb-title mb-hop"]
      [ div [class "mb-hop-name"] [text "Name"]
      , div [class "mb-hop-alpha"] [text "Alpha Acid Low"]
      , div [] [text "Alpha Acid High"]
      ]
    , div [] (List.map viewHop2 hops)
    ]

viewHop2 : Hop -> Html Msg
viewHop2 hop =
  div [ class "mb-hop"] 
    [ div [ class "mb-hop-name" ] [text hop.name]
    , div [ class "mb-hop-alpha" ] [text ((toString hop.acid.alpha.low) ++ "%")]
    , div [ class "mb-hop-alpha" ] [text ((toString hop.acid.alpha.high) ++ "%")]
    , button [ class "mb-hop-button", onClick (AddMbHop hop)] [text "add new"]
    ]

viewSrm : Srm -> Html Msg
viewSrm srm =
  div [] 
    [ label [] [ text "Volume"]
    , input [ type_ "text", defaultValue (toString srm.volume)] []
    , div [] 
            [ div [ class "ibu-title"] 
                  [ h5 [ class "ibu-input" ] [ text "Name"] 
                  , h5 [ class "ibu-input" ] [ text "Lovibond"] 
                  , h5 [ class "ibu-input" ] [ text "Amount"]
                  , h5 [ class "ibu-text" ] [ text "Morey" ]
                  , h5 [ class "ibu-text" ] [ text "Daniels" ]
                  , h5 [ class "ibu-text" ] [ text "Mosher" ]
                  ]
            , div [ class "hops" ] (List.map viewFermentable srm.fermentables)
            ]
      , div [ class "ibu-totals"] 
            [ div [] 
                [ div [ class "ibu-total"] 
                    [ h5 [] [ text "Mosher Total:" ]
                    , h5 [class "ibu-total-value"] [ text (Round.round 2 srm.mosher)]
                    ]
                , div [ class "ibu-total"] 
                      [ h5 [] [ text "Daniels Total:" ]
                      , h5 [ class "ibu-total-value"] [ text  (Round.round 2 srm.daniels)]
                      ]
                , div [ class "ibu-total"] 
                      [ h5 [] [ text "Morey Total:" ]
                      , h5 [ class "ibu-total-value"] [ text  (Round.round 2 srm.morey)]
                      ]
                ]
            , button [ class "add-button", onClick AddFermentable ] [ text "add new"]
            ]
    ]

viewFermentable : Fermentable -> Html Msg
viewFermentable fermentable =
  div [ class "hop"]
      [ input [ class "ibu-input", defaultValue fermentable.name, onInput (SetFermentableName fermentable)] []
      , input [ class "ibu-input", defaultValue (toString fermentable.lovibond), onInput (SetFermentableLovibond fermentable) ] []
      , input [ class "ibu-input", defaultValue (toString fermentable.amount), onInput (SetFermentableAmount fermentable) ] []
      , div [ class "ibu-text"] [ text (Round.round 2 fermentable.morey) ]
      , div [ class "ibu-text"] [ text (Round.round 2 fermentable.daniels) ]
      , div [ class "ibu-text"] [ text (Round.round 2 fermentable.mosher) ]
      , button [ class "remove-button", onClick (RemoveHop fermentable.index)] [ text "-"]
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

onChange : (String -> msg) -> Html.Attribute msg
onChange tagger =
  on "change" (Json.Decode.map tagger Html.Events.targetValue)



--HTTP Stuff


initialCmd : Cmd Msg
initialCmd =
  decodeHopComplete
    |> Http.get "https://api.microbrew.it/hops?from=0&size=10"
    |> Http.send LoadHops


searchHops : String -> Cmd Msg
searchHops query =
    decodeHopComplete
    |> Http.get ("https://api.microbrew.it/hops/search?query=" ++ query)
    |> Http.send LoadHops


-- Fermentable
type alias Fermentable =
    { index : Int
    , id : Int
    , name : String
    , lovibond : Float
    , amount: Float
    , morey : Float
    , daniels : Float
    , mosher : Float
    }

initFermentable : Int -> Fermentable
initFermentable index =
  {index = index, id = -1, name = "", lovibond = 30, amount = 0.0, morey = 0.0, daniels = 0.0, mosher = 0.0}

nextIndex : List {a | index : Int} -> Int
nextIndex items = 
  let 
    indexes = List.map (\item -> item.index) items
  in
    case (List.maximum indexes) of
      Nothing ->
        1
      Just number ->
        number + 1
