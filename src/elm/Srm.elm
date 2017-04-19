module Srm exposing(..)
import Formula exposing (..)
import Svg
import Svg.Attributes as SvgAtt
import Formula exposing(..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing ( onInput, onClick,on )
import Round exposing( round )


type Msg 
  = SetSrmVolume String
  | SetFermentableName Fermentable String
  | SetFermentableLovibond Fermentable String
  | SetFermentableAmount Fermentable String
  | AddFermentable
  | RemoveFermentable Int

type alias Model =
  { daniels: Float
  , fermentables: List Fermentable
  , morey : Float 
  , mosher: Float
  , volume: Float
  }



init : Model
init =
  {daniels = 0.0, fermentables = [], morey = 0.0, mosher = 0.0, volume = 20.0}

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of 
    SetSrmVolume volume ->
      let
        v = 
          case (String.toFloat volume) of
            Ok number ->
              number
            Err _ ->
              model.volume
        fermentables = List.map (recalculateSrm v) model.fermentables
        newSrm = updateSrm model fermentables
      in
        (newSrm, Cmd.none)
    SetFermentableName fermentable name ->
      let
        updatedFermentable = {fermentable | name = name }
        fermentables = List.map (updateFermentable updatedFermentable) model.fermentables
        newSrm = {model | fermentables = fermentables}
      in
        (newSrm, Cmd.none)
    SetFermentableLovibond fermentable lovibond ->
      let
        updatedFermentable = 
          case (String.toFloat lovibond) of
            Ok number ->
              {fermentable | lovibond = number}
            Err error ->
              fermentable
        fermentables = List.map (updateFermentable updatedFermentable) model.fermentables
          |> List.map (recalculateSrm model.volume)
        newSrm = updateSrm model fermentables
      in
        (newSrm, Cmd.none)
    SetFermentableAmount fermentable amount ->
      let
        updatedFermentable = 
          case (String.toFloat amount) of
            Ok number ->
              {fermentable | amount = number}
            Err error ->
              fermentable
        fermentables = List.map (updateFermentable updatedFermentable) model.fermentables
          |> List.map (recalculateSrm model.volume)
        newSrm = updateSrm model fermentables
      in
        (newSrm, Cmd.none)
    AddFermentable ->
      let
        fermentable = initFermentable (nextIndex model.fermentables)
        newSrm = { model | fermentables = (model.fermentables ++ [fermentable]) }
      in
        (newSrm, Cmd.none)
    RemoveFermentable index ->
      let
        fermentables = List.filter (\f -> f.index /= index) model.fermentables
        newSrm = { model | fermentables = fermentables}
      in
        (newSrm, Cmd.none)  

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

recalculateSrm : Float -> Fermentable -> Fermentable
recalculateSrm volume fermentable =
    {fermentable 
      | mosher = (mosher fermentable.amount fermentable.lovibond volume)
      , morey = (morey fermentable.amount fermentable.lovibond volume)
      , daniels = (mosher fermentable.amount fermentable.lovibond volume)
    }

updateFermentable : Fermentable -> Fermentable -> Fermentable
updateFermentable newFermentable oldFermentable =
  if newFermentable.index == oldFermentable.index then
    { oldFermentable 
    | name = newFermentable.name
    , lovibond = newFermentable.lovibond
    , amount = newFermentable.amount
    , morey = newFermentable.amount
    , mosher = newFermentable.mosher
    , daniels = newFermentable.daniels
    }
  else
    oldFermentable 



updateSrm : Model -> List Fermentable -> Model
updateSrm srm fermentables =
  { srm 
  | fermentables = fermentables
  , morey = (List.map (\f -> f.morey) fermentables |> List.sum)
  , mosher = (List.map (\f -> f.mosher) fermentables |> List.sum)
  , daniels = (List.map (\f -> f.daniels) fermentables |> List.sum)
  }

viewSrm : Model -> Html Msg
viewSrm srm =
  div [] 
    [ label [] [ text "Volume"]
    , input [ class "volume-input", type_ "text", defaultValue (toString srm.volume), onInput SetSrmVolume] []
    , viewTotalSrm srm
    , viewFermentableTable srm.fermentables 
    , button [onClick AddFermentable ] [ text "add"]
    ]

viewTotalSrm : Model -> Html Msg
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