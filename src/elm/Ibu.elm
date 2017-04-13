module Ibu exposing(..)
import Formula exposing(..)
import Json.Decode exposing (int, string, float, nullable,list, map, Decoder)
import Json.Decode.Pipeline exposing (decode, required, optional, hardcoded)


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