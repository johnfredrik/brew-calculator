module Components.Hop exposing(..)
import Json.Decode exposing (int, string, float, nullable,list, map, Decoder)
import Json.Decode.Pipeline exposing (decode, required, optional, hardcoded)

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

init : Int -> Hop 
init index =
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
    |> Json.Decode.Pipeline.hardcoded -1
    |> Json.Decode.Pipeline.required "hopId" int
    |> Json.Decode.Pipeline.required "name" string
    |> Json.Decode.Pipeline.hardcoded 0.0
    |> Json.Decode.Pipeline.hardcoded 0.0
    |> Json.Decode.Pipeline.hardcoded 0
    |> Json.Decode.Pipeline.hardcoded 0.0
    |> Json.Decode.Pipeline.hardcoded 0.0
    |> Json.Decode.Pipeline.hardcoded "Whole"
    |> Json.Decode.Pipeline.required "acids" decodeAcid

decodeAcid : Decoder Acid
decodeAcid =
   decode Acid
    |> required "alpha" decodeAlpha

decodeAlpha : Decoder Alpha
decodeAlpha =
  decode Alpha
    |> required "low" float
    |> required "high" float

decodeHopComplete : Decoder HopComplete
decodeHopComplete = 
  decode HopComplete
    |> required "hops" (Json.Decode.list decodeHop)

updateHop : Hop -> Hop -> Hop
updateHop newHop oldHop =
  if newHop.index == oldHop.index then
    { oldHop | name = newHop.name, aa = newHop.aa, boilTime = newHop.boilTime, amount = newHop.amount, hopType = newHop.hopType }
  else
    oldHop

nextHopIndex : List Hop -> Int
nextHopIndex hops = 
  let 
    indexes = List.map (\hop -> hop.index) hops
  in
    case (List.maximum indexes) of
      Nothing ->
        1
      Just number ->
        number + 1