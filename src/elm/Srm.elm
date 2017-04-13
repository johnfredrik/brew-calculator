module Srm exposing(..)
import Formula exposing (..)

type alias Srm =
  { daniels: Float
  , fermentables: List Fermentable
  , morey : Float 
  , mosher: Float
  , volume: Float
  }

initSrm : Srm
initSrm =
  {daniels = 0.0, fermentables = [], morey = 0.0, mosher = 0.0, volume = 20.0}

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



updateSrm : Srm -> List Fermentable -> Srm
updateSrm srm fermentables =
  { srm 
  | fermentables = fermentables
  , morey = (List.map (\f -> f.morey) fermentables |> List.sum)
  , mosher = (List.map (\f -> f.mosher) fermentables |> List.sum)
  , daniels = (List.map (\f -> f.daniels) fermentables |> List.sum)
  }