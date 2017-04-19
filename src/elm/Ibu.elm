module Ibu exposing(..)
import Formula exposing(..)
import Json.Decode exposing (int, string, float, nullable,list, map, Decoder)
import Json.Decode.Pipeline as Pipeline exposing (decode, required, optional, hardcoded)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing ( onInput, onClick,on )
import Round exposing( round )
import Http

type alias Model =
  { og : Float
  , boilVolume : Float
  , hops : List Hop
  , totalRager : Float
  , totalTinseth : Float
  , hopList : List Hop
  , error : Maybe String
  }

init : Model
init = 
  { og = 1.054, boilVolume = 20, hops = [], totalRager = 0, totalTinseth = 0, hopList = [], error = Nothing}

type Msg 
  = AddHop
  | RemoveHop Int
  | SetHopName Hop String
  | SetHopAlphaAcid Hop String
  | SetHopBoilTime Hop String
  | SetHopAmount Hop String
  | HopTypeSelected Hop String
  | LoadHops (Result Http.Error HopComplete)
  | SearchHops String
  | AddMbHop Hop

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    AddHop ->
        let
          hopId = (nextIndex model.hops)
          newIbu = { model | hops = (model.hops ++ [(newHop hopId) ])}
        in
          (newIbu, Cmd.none)
    RemoveHop index ->
        let
          hops = List.filter (\h -> h.index /= index) model.hops
                  |> List.map (recalculateIbu model.boilVolume model.og)
          newIbu = { model | hops = hops, totalRager = calculateRager hops, totalTinseth = calculateTinseth hops}
        in
          (newIbu, Cmd.none)
    SetHopName hop name ->
        let
          updatedHop = {hop | name = name}
          hops = List.map (updateHop updatedHop) model.hops
          newIbu = { model | hops = hops}
        in
          (newIbu, Cmd.none)
    SetHopAlphaAcid hop alphaAcid ->
      let
        updatedHop = 
          case (String.toFloat alphaAcid) of
            Ok number ->
              { hop | aa = number }
            Err error ->
              hop
        hops = List.map (updateHop updatedHop) model.hops
                |> List.map (recalculateIbu model.boilVolume model.og)
        newIbu = { model | hops = hops, totalRager = calculateRager hops, totalTinseth = calculateTinseth hops}
      in
        (newIbu, Cmd.none)
    SetHopBoilTime hop boilTime ->
      let
        updatedHop = 
          case (String.toInt boilTime) of
            Ok number ->
              {hop | boilTime = number}
            Err error ->
              hop
        hops = List.map (updateHop updatedHop) model.hops
                |> List.map (recalculateIbu model.boilVolume model.og)
        newIbu = { model | hops = hops, totalRager = calculateRager hops, totalTinseth = calculateTinseth hops}
      in
        (newIbu, Cmd.none)
    SetHopAmount hop amount ->
      let
        updatedHop = 
          case (String.toFloat amount) of
            Ok number ->
              {hop | amount = number}
            Err error ->
              hop
        hops = List.map (updateHop updatedHop) model.hops
                |> List.map (recalculateIbu model.boilVolume model.og)
        newIbu = { model | hops = hops, totalRager = calculateRager hops, totalTinseth = calculateTinseth hops}
      in
        (newIbu, Cmd.none)
    HopTypeSelected hop hopType ->
      let
        updatedHop = 
          {hop | hopType = hopType}
        hops = List.map (updateHop updatedHop) model.hops
                |> List.map (recalculateIbu model.boilVolume model.og)
        newIbu = { model | hops = hops, totalRager = calculateRager hops, totalTinseth = calculateTinseth hops}
      in
        (newIbu, Cmd.none)
    AddMbHop hop ->
      let
        newHop = { index = (nextIndex model.hops),id = hop.id, name = hop.name, aa = hop.acid.alpha.low, amount = 0, boilTime = 0, rager = 0, tinseth = 0, hopType = "Whole", acid = initAcid}
        newIbu = { model | hops = (model.hops ++ [newHop])}
      in
        (newIbu, Cmd.none)
    LoadHops (Ok hopComplete) ->  
      let
        newIbu = { model | hopList = hopComplete.hops}
      in
        (newIbu, Cmd.none)
    LoadHops (Err _) ->
      ({ model | error = Just "Loading hops failed"}, Cmd.none)
    SearchHops query ->
      if (String.length query) > 2 then
        (model, searchHops query)
      else if (String.length query) == 0 then
        (model, initialCmd)
      else 
        (model, Cmd.none)

type alias Hop =
  { index : Int
  , id : Int
  , name : String
  , aa : Float
  , amount : Float
  , boilTime : Int
  , rager : Float
  , tinseth : Float
  , hopType : String
  , acid : Acid
  }

newHop : Int -> Hop 
newHop index =
  { index = index
  , id = -1, name = ""
  , aa = 0.0, amount = 0
  , boilTime = 0, rager = 0
  , tinseth = 0
  , hopType = "Whole"
  , acid =  initAcid 
  }

initAcid : Acid
initAcid =
  { alpha = { low = 0, high = 0} }

type alias Acid =
  { alpha : Alpha }

type alias Alpha =
  { low: Float, high: Float}


type alias HopComplete = 
  { hops: List Hop }

decodeHop : Decoder Hop
decodeHop =
  decode Hop
    |> Pipeline.hardcoded -1
    |> Pipeline.required "hopId" int
    |> Pipeline.required "name" string
    |> Pipeline.hardcoded 0.0
    |> Pipeline.hardcoded 0.0
    |> Pipeline.hardcoded 0
    |> Pipeline.hardcoded 0.0
    |> Pipeline.hardcoded 0.0
    |> Pipeline.hardcoded "Whole"
    |> Pipeline.required "acids" decodeAcid

decodeAcid : Decoder Acid
decodeAcid =
   decode Acid
    |> Pipeline.required "alpha" decodeAlpha

decodeAlpha : Decoder Alpha
decodeAlpha =
  decode Alpha
    |> Pipeline.required "low" float
    |> Pipeline.required "high" float

decodeHopComplete : Decoder HopComplete
decodeHopComplete = 
  decode HopComplete
    |> Pipeline.required "hops" (Json.Decode.list decodeHop)

updateHop : Hop -> Hop -> Hop
updateHop newHop oldHop =
  if newHop.index == oldHop.index then
    { oldHop | name = newHop.name, aa = newHop.aa, boilTime = newHop.boilTime, amount = newHop.amount, hopType = newHop.hopType }
  else
    oldHop

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

searchHops : String -> Cmd Msg
searchHops query =
    decodeHopComplete
    |> Http.get ("https://api.microbrew.it/hops/search?query=" ++ query)
    |> Http.send LoadHops

initialCmd : Cmd Msg
initialCmd =
  decodeHopComplete
    |> Http.get "https://api.microbrew.it/hops?from=0&size=10"
    |> Http.send LoadHops


--View
viewIbu : Model -> Html Msg
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

onChange : (String -> msg) -> Html.Attribute msg
onChange tagger =
  on "change" (Json.Decode.map tagger Html.Events.targetValue)
