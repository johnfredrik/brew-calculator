module Components.Formula exposing(..)

miller : Float -> Float -> Float
miller og fg =
    (og - fg) / 0.75 * 100

simple : Float -> Float -> Float
simple og fg =
  (og - fg) * 131.25

alternativeSimple : Float -> Float -> Float
alternativeSimple og fg =
  ((1.05 / 0.79)*((og-fg) / fg))*100

advanced : Float -> Float -> Float
advanced og fg =
  (og-fg)*(100.3*(og-fg) + 125.65)

alternativeAdvanced : Float -> Float -> Float 
alternativeAdvanced og fg =
  (76.08 * (og-fg) / (1.775-og)) * (fg / 0.794)

microbrewit : Float -> Float -> Float
microbrewit og fg =
  ((alternativeSimple og fg) + (alternativeAdvanced og fg) + (simple og fg) +  (advanced og fg) + (miller og  fg)) / 5
