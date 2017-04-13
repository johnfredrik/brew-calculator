module Main exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing ( onInput, onClick,on )
import Round exposing( round )
import Formula exposing(..)
import Ibu exposing(..)
import Srm exposing(..)
import Abv exposing(..)
import String
import Json.Decode exposing (map)
import Http
import Svg
import Svg.Attributes as SvgAtt


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




type Calculator
  = AbvCalculator
  | IbuCalculator
  | SrmCalculator

--   UPDATE
type Msg 
  = NoOp
  | ChangeCalculator Calculator
  | SetOG String
  | SetFG String
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
  | SetSrmVolume String
  | SetFermentableName Fermentable String
  | SetFermentableLovibond Fermentable String
  | SetFermentableAmount Fermentable String
  | AddFermentable
  | RemoveFermentable Int

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
        newIbu = { oldIbu | hops = (oldIbu.hops ++ [(Ibu.newHop hopId) ])}
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
    SetSrmVolume volume ->
      let
        oldSrm = model.srm
        v = 
          case (String.toFloat volume) of
            Ok number ->
              number
            Err _ ->
              oldSrm.volume
        fermentables = List.map (recalculateSrm v) oldSrm.fermentables
        newSrm = updateSrm oldSrm fermentables
      in
        ( {model | srm = newSrm}, Cmd.none)
    SetFermentableName fermentable name ->
      let
        updatedFermentable = {fermentable | name = name }
        oldSrm = model.srm
        fermentables = List.map (updateFermentable updatedFermentable) oldSrm.fermentables
        newSrm = {oldSrm | fermentables = fermentables}
      in
        ({model | srm = newSrm}, Cmd.none)
    SetFermentableLovibond fermentable lovibond ->
      let
        updatedFermentable = 
          case (String.toFloat lovibond) of
            Ok number ->
              {fermentable | lovibond = number}
            Err error ->
              fermentable
        oldSrm = model.srm
        fermentables = List.map (updateFermentable updatedFermentable) oldSrm.fermentables
          |> List.map (recalculateSrm oldSrm.volume)
        newSrm = updateSrm oldSrm fermentables
      in
        ({model | srm = newSrm}, Cmd.none)
    SetFermentableAmount fermentable amount ->
      let
        updatedFermentable = 
          case (String.toFloat amount) of
            Ok number ->
              {fermentable | amount = number}
            Err error ->
              fermentable
        oldSrm = model.srm
        fermentables = List.map (updateFermentable updatedFermentable) oldSrm.fermentables
          |> List.map (recalculateSrm oldSrm.volume)
        newSrm = updateSrm oldSrm fermentables
      in
        ({model | srm = newSrm}, Cmd.none)
    AddFermentable ->
      let
        fermentable = initFermentable (nextIndex model.srm.fermentables)
        oldSrm = model.srm
        newSrm = { oldSrm | fermentables = (oldSrm.fermentables ++ [fermentable]) }
      in
        ({model | srm = newSrm}, Cmd.none)
    RemoveFermentable index ->
      let
        oldSrm = model.srm
        fermentables = List.filter (\f -> f.index /= index) model.srm.fermentables
        newSrm = { oldSrm | fermentables = fermentables}
      in
        ({ model | srm = newSrm}, Cmd.none)  


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
      , (viewHopTable ibu.hops)
      , button [ class "add-button", onClick AddHop ] [ text "add new"]
      , viewHopList ibu.hopList
      ]

viewHopTable : List Hop -> Html Msg
viewHopTable hops =
  table [ class "rwd-table"] (
   tr [] 
      [ th [] [ text "Name"]
      , th [] [ text "Alpha Acids"]
      , th [] [ text "Amount"]
      , th [] [ text "BoilTime" ]
      , th [] [ text "Type" ]
      , th [] [ text "Rager" ]
      , th [] [ text "Tinseth"]
      ]
     :: (List.map viewHopRow hops))

viewHopRow : Hop -> Html Msg
viewHopRow hop = 
  tr []
      [ td [ attribute "data-th" "Name" ] [ input [ defaultValue hop.name, onInput (SetHopName hop)] [] ]
      , td [ attribute "data-th" "Alpha Acids"] [ input [ defaultValue (toString hop.aa), onInput (SetHopAlphaAcid hop) ] [] ]
      , td [ attribute "data-th" "Amount"] [ input [ defaultValue (toString hop.amount), onInput (SetHopAmount hop) ] [] ]
      , td [ attribute "data-th" "BoilTime"] [ input [ defaultValue (toString hop.boilTime), onInput (SetHopBoilTime hop) ] [] ]
      , td [ attribute "data-th" "Type" ] 
        [
          select [ onChange (HopTypeSelected hop)] 
          [ option [ selected (hop.hopType == "Whole")] [ text "Whole"]
          , option [ selected (hop.hopType == "Pellet") ] [ text "Pellet"]
          ]
        ]
      , td [ attribute "data-th" "Rager" ] [ text (Round.round 2 hop.rager) ]
      , td [ attribute "data-th" "Tinseth" ] [ text (Round.round 2 hop.tinseth) ]
      , td [] [ button [ class "remove-button", onClick (RemoveHop hop.index)] [ text "Remove"]]
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
  div []
    [ input [ placeholder "search hop", onInput SearchHops] []
    , div [] (List.map viewHop hops)
    ]

viewHop : Hop -> Html Msg
viewHop hop =
  div [] 
    [ span [] [text hop.name]
    , span [] [text ("(" ++ (toString hop.acid.alpha.low) ++ " - " ++(toString hop.acid.alpha.high) ++ ")")]
    , button [ onClick (AddMbHop hop)] [text "add"]
    ]

viewSrm : Srm -> Html Msg
viewSrm srm =
  div [] 
    [ label [] [ text "Volume"]
    , input [ class "volume-input", type_ "text", defaultValue (toString srm.volume), onInput SetSrmVolume] []
    , viewTotalSrm srm
    , viewFermentableTable srm.fermentables 
    , button [onClick AddFermentable ] [ text "add"]
    ]

viewTotalSrm : Srm -> Html Msg
viewTotalSrm srm =
  div [ class "srm-total"] 
    [ div [] 
      [ span [] [text ("Mosher: " ++ (Round.round 2 srm.mosher))]
      , (viewCircle (getColor srm.mosher))
      ]
    , div [] [ text ("Daniels: " ++ (Round.round 2 srm.daniels))]
    , div [] [ text ("Morey: " ++ (Round.round 2 srm.morey))]
    ]

viewFermentableTable : List Fermentable -> Html Msg
viewFermentableTable fermentables =
  table [class "rwd-table"] 
          (tr [] 
              [ th [] [ text "Name"] 
              , th [] [ text "Lovibond"] 
              , th [] [ text "Amount"]
              , th [] [ text "Morey" ]
              , th [] [ text "Daniels" ]
              , th [] [ text "Mosher" ]
              ]
          :: (List.map viewFermentableRow fermentables))



viewFermentableRow : Fermentable -> Html Msg
viewFermentableRow fermentable =
  tr []
      [ td [ attribute "data-th" "Name"] [ input [ defaultValue fermentable.name, onInput (SetFermentableName fermentable)] []]
      , td [ attribute "data-th" "Lovibond"] [ input [ defaultValue (toString fermentable.lovibond), onInput (SetFermentableLovibond fermentable) ] []]
      , td [ attribute "data-th" "Amount"] [ input [ defaultValue (toString fermentable.amount), onInput (SetFermentableAmount fermentable) ] []]
      , td [ attribute "data-th" "Morey"]
        [ span [] [text (Round.round 2 fermentable.morey)]
        , (viewCircle (getColor fermentable.morey))
        ]
      , td [ attribute "data-th" "Daniels"]
        [ span [] [text (Round.round 2 fermentable.daniels)]
        , (viewCircle (getColor fermentable.daniels))
        ]
      , td [ attribute "data-th" "Mosher"]
        [ span [] [text (Round.round 2 fermentable.mosher)]
        , (viewCircle (getColor fermentable.mosher))
        ]
      , td [] [ button [onClick (RemoveFermentable fermentable.index)] [ text "remove"]]
      ]

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


nextIndex : List {a | index : Int} -> Int
nextIndex items = 
  let 
    indexes = List.map .index items
  in
    case (List.maximum indexes) of
      Nothing ->
        1
      Just number ->
        number + 1

viewCircle : String -> Html Msg
viewCircle color = 
    Svg.svg 
      [ SvgAtt.width "20"
      , SvgAtt.height "20"
      ]
      [ Svg.circle 
          [ SvgAtt.cx "10"
          , SvgAtt.cy "10"
          , SvgAtt.r "8"
          , SvgAtt.stroke "black"
          , SvgAtt.strokeWidth "1"
          , SvgAtt.fill color
          ]
          []
      ]