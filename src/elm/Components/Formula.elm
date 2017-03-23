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


--IBU

tanh : Float -> Float
tanh x =
    let
      value = e^(2*x)
    in
      (value-1) / (value+1)

rager : Float -> Float -> Float -> Float -> Float -> Float
rager boilTime amount aa boilVolume boilGravity =
    let
      utilisation = ragerUtilisation boilTime
    in
      ragerIbu amount utilisation aa boilVolume boilGravity


ragerUtilisation : Float -> Float
ragerUtilisation boilTime =
    let
      util = 18.11 + 13.86 * tanh((boilTime-31.32) / 18.27)
    in
      util / 100
      
  
ragerIbu : Float -> Float -> Float -> Float -> Float -> Float
ragerIbu amount utilisation aa boilVolume boilGravity =
  let
    ga = 
      if boilGravity > 1.050 then
        (boilGravity - 1.050) / 0.2
      else
        0
    alphaAcid = aa / 100
  in
    (amount * utilisation * alphaAcid * 1000) / (boilVolume * (1+ga))

tinseth : Float -> Float -> Float -> Float -> Float -> Float
tinseth boilTime amount aa boilVolume boilGravity = 
  let
    mgl = tinsethMgl amount aa boilVolume
    utilisation = tinsetUtilisation boilTime boilGravity
  in
    utilisation * mgl

tinsethMgl : Float -> Float -> Float -> Float 
tinsethMgl amount aa boilVolume =
    ((aa/100.0) * amount * 1000) / boilVolume

tinsetUtilisation : Float -> Float -> Float
tinsetUtilisation boilTime boilGravity =
  let
    bignessFactor = (1.65 * (0.000125^(boilGravity - 1.0)))
    boilTimeFactor = (1 - (e^(-0.04 * boilTime))) / 4.15
  in
    bignessFactor * boilTimeFactor
    

