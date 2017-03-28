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

hop : Int -> Hop 
hop index =
  {index = index, id = -1, name = "", aa = 0.0, amount = 0, boilTime = 0, rager = 0, tinseth = 0, hopType = "Whole", acid =  initAcid }

initAcid : Acid
initAcid =
  { alpha = { low = 0, high = 0} }


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

decodeHopComplete : Decoder HopComplete
decodeHopComplete = 
  Json.Decode.map
    HopComplete
    (Json.Decode.field "hops" (Json.Decode.list decodeHop))

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