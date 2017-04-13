module Abv exposing(..)
import Formula exposing(..)

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