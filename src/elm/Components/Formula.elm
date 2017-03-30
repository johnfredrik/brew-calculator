module Components.Formula exposing(..)
import Dict
import Round exposing (round)
import Debug exposing (log)

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

mcu: Float -> Float -> Float -> Float
mcu weight lovibond postBoilVolume =
  let
    value = (weight*lovibond) / postBoilVolume
  in
    if isInfinite value then
      0
    else 
      value

morey: Float -> Float -> Float -> Float
morey weight lovibond postBoilVolume =
    1.4922 * ((mcu weight lovibond postBoilVolume)^0.6859)

daniels: Float -> Float -> Float -> Float
daniels weight lovibond postBoilVolume =
  (0.2 * (mcu weight lovibond postBoilVolume)) + 8.4

mosher: Float -> Float -> Float -> Float
mosher weight lovibond postBoilVolume =
  (0.3 * (mcu weight lovibond postBoilVolume)) + 4.7
  

type alias Rgb = 
  { r : Int
  , g : Int
  , b : Int
  }

rgbToCss : Rgb -> String
rgbToCss rgb =
  let
    color = "rgb(" ++ (toString rgb.r) ++ "," ++ (toString  rgb.g) ++ "," ++ (toString rgb.b) ++ ")"  
    d = log "color" color
  in
    color
  

getColor : Float -> String
getColor srm =
  let
    rgb = 
      if srm > 40.0 then 
        Rgb 3 4 3
      else if srm <= 0.0 then 
        Rgb 248 248 230
      else
        case (Dict.get (Round.round 1 srm) srmRgbDict) of
          Just value ->
            value
          Nothing ->
            Rgb 248 248 230
  in
    rgbToCss rgb

srmRgbDict : Dict.Dict String Rgb
srmRgbDict = Dict.fromList
    [ ("0.1", Rgb 248 248 230 )
    , ("0.2", Rgb 248 248 220 )
    , ("0.3", Rgb 247 247 199 )
    , ("0.4", Rgb 244 249 185 )
    , ("0.5", Rgb 247 249 180 )
    , ("0.6", Rgb 248 249 178 )
    , ("0.7", Rgb 244 246 169 )
    , ("0.8", Rgb 245 247 166 )
    , ("0.9", Rgb 246 247 156 )
    , ("1.0", Rgb 243 249 147 )
    , ("1.1", Rgb 246 248 141 )
    , ("1.2", Rgb 246 249 136 )
    , ("1.3", Rgb 245 250 128 )
    , ("1.4", Rgb 246 249 121 )
    , ("1.5", Rgb 248 249 114 )
    , ("1.6", Rgb 243 249 104 )
    , ("1.7", Rgb 246 248 107 )
    , ("1.8", Rgb 248 247 99 )
    , ("1.9", Rgb 245 247 92 )
    , ("2.0", Rgb 248 247 83 )
    , ("2.1", Rgb 244 248 72 )
    , ("2.2", Rgb 248 247 73 )
    , ("2.3", Rgb 246 247 62 )
    , ("2.4", Rgb 241 248 53 )
    , ("2.5", Rgb 244 247 48 )
    , ("2.6", Rgb 246 249 40 )
    , ("2.7", Rgb 243 249 34 )
    , ("2.8", Rgb 245 247 30 )
    , ("2.9", Rgb 248 245 22 )
    , ("3.0", Rgb 246 245 19 ) 
    , ("3.1", Rgb 244 242 2)
    , ("3.2", Rgb 244 240 21)
    , ("3.3", Rgb 243 242 19)
    , ("3.4", Rgb 244 238 24)
    , ("3.5", Rgb 244 237 29)
    , ("3.6", Rgb 238 233 22)
    , ("3.7", Rgb 240 233 23)
    , ("3.8", Rgb 238 231 25)
    , ("3.9", Rgb 234 230 21)
    , ("4.0", Rgb 236 230 26)
    , ("4.1", Rgb 230 225 24)
    , ("4.2", Rgb 232 225 25)
    , ("4.3", Rgb 230 221 27)
    , ("4.4", Rgb 224 218 23)
    , ("4.5", Rgb 229 216 31)
    , ("4.6", Rgb 229 214 30)
    , ("4.7", Rgb 223 213 26)
    , ("4.8", Rgb 226 213 28)
    , ("4.9", Rgb 223 209 29)
    , ("5.0", Rgb 224 208 27)
    , ("5.1", Rgb 224 204 32)
    , ("5.2", Rgb 221 204 33)
    , ("5.3", Rgb 220 203 29)
    , ("5.4", Rgb 218 200 32)
    , ("5.5", Rgb 220 197 34)
    , ("5.6", Rgb 218 196 41)
    , ("5.7", Rgb 217 194 43)
    , ("5.8", Rgb 216 192 39)
    , ("5.9", Rgb 213 190 37)
    , ("6.0", Rgb 213 188 38)
    , ("6.1", Rgb 212 184 39)
    , ("6.2", Rgb 214 183 43)
    , ("6.3", Rgb 213 180 45)
    , ("6.4", Rgb 210 179 41)
    , ("6.5", Rgb 208 178 42)
    , ("6.6", Rgb 208 176 46)
    , ("6.7", Rgb 204 172 48)
    , ("6.8", Rgb 204 172 52)
    , ("6.9", Rgb 205 170 55)
    , ("7.0", Rgb 201 167 50)
    , ("7.1", Rgb 202 167 52)
    , ("7.2", Rgb 201 166 51)
    , ("7.3", Rgb 199 162 54)
    , ("7.4", Rgb 198 160 56)
    , ("7.5", Rgb 200 158 60)
    , ("7.6", Rgb 194 156 54)
    , ("7.7", Rgb 196 155 54)
    , ("7.8", Rgb 198 151 60)
    , ("7.9", Rgb 193 150 60)
    , ("8.0", Rgb 191 146 59)
    , ("8.1", Rgb 190 147 57)
    , ("8.2", Rgb 190 147 59)
    , ("8.3", Rgb 190 145 60)
    , ("8.4", Rgb 186 148 56)
    , ("8.5", Rgb 190 145 58)
    , ("8.6", Rgb 193 145 59)
    , ("8.7", Rgb 190 145 58)
    , ("8.8", Rgb 191 143 59)
    , ("8.9", Rgb 191 141 61)
    , ("9.0", Rgb 190 140 58)
    , ("9.1", Rgb 192 140 61)
    , ("9.2", Rgb 193 138 62)
    , ("9.3", Rgb 192 137 59)
    , ("9.4", Rgb 193 136 59)
    , ("9.5", Rgb 195 135 63)
    , ("9.6", Rgb 191 136 58)
    , ("9.7", Rgb 191 134 67)
    , ("9.8", Rgb 193 131 67)
    , ("9.9", Rgb 190 130 58)
    , ("10.0", Rgb 191 129 58)
    , ("10.1", Rgb 191 131 57)
    , ("10.2", Rgb 191 129 58)
    , ("10.3", Rgb 191 129 58)
    , ("10.4", Rgb 190 129 55)
    , ("10.5", Rgb 191 127 59)
    , ("10.6", Rgb 194 126 59)
    , ("10.7", Rgb 188 128 54)
    , ("10.8", Rgb 190 124 55)
    , ("10.9", Rgb 193 122 55)
    , ("11.0", Rgb 190 124 55)
    , ("11.1", Rgb 194 121 59)
    , ("11.2", Rgb 193 120 56)
    , ("11.3", Rgb 190 119 52)
    , ("11.4", Rgb 182 117 54)
    , ("11.5", Rgb 196 116 59)
    , ("11.6", Rgb 191 118 56)
    , ("11.7", Rgb 190 116 57)
    , ("11.8", Rgb 191 115 58)
    , ("11.9", Rgb 189 115 56)
    , ("12.0", Rgb 191 113 56)
    , ("12.1", Rgb 191 113 53)
    , ("12.2", Rgb 188 112 57)
    , ("12.3", Rgb 190 112 55)
    , ("12.4", Rgb 184 110 52)
    , ("12.5", Rgb 188 109 55)
    , ("12.6", Rgb 189 109 55)
    , ("12.7", Rgb 186 106 50)
    , ("12.8", Rgb 190 103 52)
    , ("12.9", Rgb 189 104 54)
    , ("13.0", Rgb 188 103 51)
    , ("13.1", Rgb 188 103 51)
    , ("13.2", Rgb 186 101 51)
    , ("13.3", Rgb 186 102 56)
    , ("13.4", Rgb 185 100 56)
    , ("13.5", Rgb 185 98 59)
    , ("13.6", Rgb 183 98 54)
    , ("13.7", Rgb 181 100 53)
    , ("13.8", Rgb 182 97 55)
    , ("13.9", Rgb 177 97 51)
    , ("14.0", Rgb 178 96 51)
    , ("14.1", Rgb 176 96 49)
    , ("14.2", Rgb 177 96 55)
    , ("14.3", Rgb 178 95 55)
    , ("14.4", Rgb 171 94 55)
    , ("14.5", Rgb 171 92 56)
    , ("14.6", Rgb 172 93 59)
    , ("14.7", Rgb 168 92 55)
    , ("14.8", Rgb 169 90 54)
    , ("14.9", Rgb 168 88 57)
    , ("15.0", Rgb 165 89 54)
    , ("15.1", Rgb 166 88 54)
    , ("15.2", Rgb 165 88 58)
    , ("15.3", Rgb 161 88 52)
    , ("15.4", Rgb 163 85 55)
    , ("15.5", Rgb 160 86 56)
    , ("15.6", Rgb 158 85 57)
    , ("15.7", Rgb 158 86 54)
    , ("15.8", Rgb 159 84 57)
    , ("15.9", Rgb 156 83 53)
    , ("16.0", Rgb 152 83 54)
    , ("16.1", Rgb 150 83 55)
    , ("16.2", Rgb 150 81 56)
    , ("16.3", Rgb 146 81 56)
    , ("16.4", Rgb 147 79 54)
    , ("16.5", Rgb 147 79 55)
    , ("16.6", Rgb 146 78 54)
    , ("16.7", Rgb 142 77 51)
    , ("16.8", Rgb 143 79 53)
    , ("16.9", Rgb 142 77 54)
    , ("17.0", Rgb 141 76 50)
    , ("17.1", Rgb 140 75 50)
    , ("17.2", Rgb 138 73 49)
    , ("17.3", Rgb 135 70 45)
    , ("17.4", Rgb 136 71 49)
    , ("17.5", Rgb 140 72 49)
    , ("17.6", Rgb 128 70 45)
    , ("17.7", Rgb 129 71 46)
    , ("17.8", Rgb 130 69 47)
    , ("17.9", Rgb 123 69 45)
    , ("18.0", Rgb 124 69 45)
    , ("18.1", Rgb 121 66 40)
    , ("18.2", Rgb 120 67 40)
    , ("18.3", Rgb 119 64 38)
    , ("18.4", Rgb 116 63 34)
    , ("18.5", Rgb 120 63 35)
    , ("18.6", Rgb 120 62 37)
    , ("18.7", Rgb 112 63 35)
    , ("18.8", Rgb 111 62 36)
    , ("18.9", Rgb 109 60 34)
    , ("19.0", Rgb 107 58 30)
    , ("19.1", Rgb 106 57 31)
    , ("19.2", Rgb 107 56 31)
    , ("19.3", Rgb 105 56 28)
    , ("19.4", Rgb 105 56 28)
    , ("19.5", Rgb 104 52 31)
    , ("19.6", Rgb 102 53 27)
    , ("19.7", Rgb 100 53 26)
    , ("19.8", Rgb 99 52 25)
    , ("19.9", Rgb 93 53 24)
    , ("20.0", Rgb 93 52 26)
    , ("20.1", Rgb 89 49 20)
    , ("20.2", Rgb 90 50 21)
    , ("20.3", Rgb 91 48 20)
    , ("20.4", Rgb 83 48 15)
    , ("20.5", Rgb 88 48 17)
    , ("20.6", Rgb 86 46 17)
    , ("20.7", Rgb 81 45 15)
    , ("20.8", Rgb 83 44 15)
    , ("20.9", Rgb 81 45 15)
    , ("21.0", Rgb 78 42 12)
    , ("21.1", Rgb 77 43 12)
    , ("21.2", Rgb 75 41 12)
    , ("21.3", Rgb 74 41 5)
    , ("21.4", Rgb 78 40 23)
    , ("21.5", Rgb 83 43 46)
    , ("21.6", Rgb 78 43 41)
    , ("21.7", Rgb 78 40 41)
    , ("21.8", Rgb 76 41 41)
    , ("21.9", Rgb 74 39 39)
    , ("22.0", Rgb 74 39 39)
    , ("22.1", Rgb 69 39 35)
    , ("22.2", Rgb 70 37 37)
    , ("22.3", Rgb 68 38 36)
    , ("22.4", Rgb 64 35 34)
    , ("22.5", Rgb 64 35 34)
    , ("22.6", Rgb 62 33 32)
    , ("22.7", Rgb 58 33 31)
    , ("22.8", Rgb 61 33 31)
    , ("22.9", Rgb 58 33 33)
    , ("23.0", Rgb 54 31 27)
    , ("23.1", Rgb 52 29 28)
    , ("23.2", Rgb 52 29 28)
    , ("23.3", Rgb 49 28 27)
    , ("23.4", Rgb 48 27 26)
    , ("23.5", Rgb 48 27 26)
    , ("23.6", Rgb 44 25 25)
    , ("23.7", Rgb 44 25 23)
    , ("23.8", Rgb 42 24 26)
    , ("23.9", Rgb 40 23 22)
    , ("24.0", Rgb 38 23 22)
    , ("24.1", Rgb 38 23 22)
    , ("24.2", Rgb 38 23 22)
    , ("24.3", Rgb 38 23 22)
    , ("24.4", Rgb 38 23 22)
    , ("24.5", Rgb 38 23 22)
    , ("24.6", Rgb 38 23 22)
    , ("24.7", Rgb 38 23 22)
    , ("24.8", Rgb 38 23 22)
    , ("24.9", Rgb 38 23 22)
    , ("25.0", Rgb 38 23 22)
    , ("25.1", Rgb 38 23 22)
    , ("25.2", Rgb 38 23 22)
    , ("25.3", Rgb 38 23 22)
    , ("25.4", Rgb 38 23 22)
    , ("25.5", Rgb 38 23 22)
    , ("25.6", Rgb 38 23 24)
    , ("25.7", Rgb 25 16 15)
    , ("25.8", Rgb 25 16 15)
    , ("25.9", Rgb 25 16 15)
    , ("26.0", Rgb 25 16 15)
    , ("26.1", Rgb 25 16 15)
    , ("26.2", Rgb 25 16 15)
    , ("26.3", Rgb 25 16 15)
    , ("26.4", Rgb 25 16 15)
    , ("26.5", Rgb 25 16 15)
    , ("26.6", Rgb 25 16 15)
    , ("26.7", Rgb 25 16 15)
    , ("26.8", Rgb 25 16 15)
    , ("26.9", Rgb 25 16 15)
    , ("27.0", Rgb 25 16 15)
    , ("27.1", Rgb 25 16 15)
    , ("27.2", Rgb 25 16 15)
    , ("27.3", Rgb 18 13 12)
    , ("27.4", Rgb 18 13 12)
    , ("27.5", Rgb 18 13 12)
    , ("27.6", Rgb 18 13 12)
    , ("27.7", Rgb 18 13 12)
    , ("27.8", Rgb 18 13 12)
    , ("27.9", Rgb 18 13 12)
    , ("28.0", Rgb 18 13 12)
    , ("28.1", Rgb 18 13 12)
    , ("28.2", Rgb 18 13 12)
    , ("28.3", Rgb 18 13 12)
    , ("28.4", Rgb 18 13 12)
    , ("28.5", Rgb 18 13 12)
    , ("28.6", Rgb 18 13 12)
    , ("28.7", Rgb 17 13 10)
    , ("28.8", Rgb 18 13 12)
    , ("28.9", Rgb 16 11 10)
    , ("29.0", Rgb 16 11 10)
    , ("29.1", Rgb 16 11 10)
    , ("29.2", Rgb 16 11 10)
    , ("29.3", Rgb 16 11 10)
    , ("29.4", Rgb 16 11 10)
    , ("29.5", Rgb 16 11 10)
    , ("29.6", Rgb 16 11 10)
    , ("29.7", Rgb 16 11 10)
    , ("29.8", Rgb 16 11 10)
    , ("29.9", Rgb 16 11 10)
    , ("30.0", Rgb 16 11 10)
    , ("30.1", Rgb 16 11 10)
    , ("30.2", Rgb 16 11 10)
    , ("30.3", Rgb 16 11 10)
    , ("30.4", Rgb 16 11 10)
    , ("30.5", Rgb 14 9 8)
    , ("30.6", Rgb 15 10 9)
    , ("30.7", Rgb 14 9 8)
    , ("30.8", Rgb 14 9 8)
    , ("30.9", Rgb 14 9 8)
    , ("31.0", Rgb 14 9 8)
    , ("31.1", Rgb 14 9 8)
    , ("31.2", Rgb 14 9 8)
    , ("31.3", Rgb 14 9 8)
    , ("31.4", Rgb 14 9 8)
    , ("31.5", Rgb 14 9 8)
    , ("31.6", Rgb 14 9 8)
    , ("31.7", Rgb 14 9 8)
    , ("31.8", Rgb 14 9 8)
    , ("31.9", Rgb 14 9 8)
    , ("32.0", Rgb 15 11 8)
    , ("32.1", Rgb 12 9 7)
    , ("32.2", Rgb 12 9 7)
    , ("32.3", Rgb 12 9 7)
    , ("32.4", Rgb 12 9 7)
    , ("32.5", Rgb 12 9 7)
    , ("32.6", Rgb 12 9 7)
    , ("32.7", Rgb 12 9 7)
    , ("32.8", Rgb 12 9 7)
    , ("32.9", Rgb 12 9 7)
    , ("33.0", Rgb 12 9 7)
    , ("33.1", Rgb 12 9 7)
    , ("33.2", Rgb 12 9 7)
    , ("33.3", Rgb 12 9 7)
    , ("33.4", Rgb 12 9 7)
    , ("33.5", Rgb 12 9 7)
    , ("33.6", Rgb 12 9 7)
    , ("33.7", Rgb 10 7 7)
    , ("33.8", Rgb 10 7 5)
    , ("33.9", Rgb 8 7 7)
    , ("34.0", Rgb 8 7 7)
    , ("34.1", Rgb 8 7 7)
    , ("34.2", Rgb 8 7 7)
    , ("34.3", Rgb 8 7 7)
    , ("34.4", Rgb 8 7 7)
    , ("34.5", Rgb 8 7 7)
    , ("34.6", Rgb 8 7 7)
    , ("34.7", Rgb 8 7 7)
    , ("34.8", Rgb 8 7 7)
    , ("34.9", Rgb 8 7 7)
    , ("35.0", Rgb 8 7 7)
    , ("35.1", Rgb 8 7 7)
    , ("35.2", Rgb 8 7 7)
    , ("35.3", Rgb 7 6 6)
    , ("35.4", Rgb 7 6 6)
    , ("35.5", Rgb 7 6 6)
    , ("35.6", Rgb 7 6 6)
    , ("35.7", Rgb 7 6 6)
    , ("35.8", Rgb 7 6 6)
    , ("35.9", Rgb 7 6 6)
    , ("36.0", Rgb 7 6 6)
    , ("36.1", Rgb 7 6 6)
    , ("36.2", Rgb 7 6 6)
    , ("36.3", Rgb 7 6 6)
    , ("36.4", Rgb 7 6 6)
    , ("36.5", Rgb 7 6 6)
    , ("36.6", Rgb 7 6 6)
    , ("36.7", Rgb 7 7 4)
    , ("36.8", Rgb 6 6 3)
    , ("36.9", Rgb 6 5 5)
    , ("37.0", Rgb 4 5 4)
    , ("37.1", Rgb 4 5 4)
    , ("37.2", Rgb 4 5 4)
    , ("37.3", Rgb 4 5 4)
    , ("37.4", Rgb 4 5 4)
    , ("37.5", Rgb 4 5 4)
    , ("37.6", Rgb 4 5 4)
    , ("37.7", Rgb 4 5 4)
    , ("37.8", Rgb 4 5 4)
    , ("37.9", Rgb 4 5 4)
    , ("38.0", Rgb 4 5 4)
    , ("38.1", Rgb 4 5 4)
    , ("38.2", Rgb 4 5 4)
    , ("38.3", Rgb 4 5 4)
    , ("38.4", Rgb 4 5 4)
    , ("38.5", Rgb 3 4 3)
    , ("38.6", Rgb 4 5 4)
    , ("38.7", Rgb 3 4 3)
    , ("38.8", Rgb 3 4 3)
    , ("38.9", Rgb 3 4 3)
    , ("39.0", Rgb 3 4 3)
    , ("39.1", Rgb 3 4 3)
    , ("39.2", Rgb 3 4 3)
    , ("39.3", Rgb 3 4 3)
    , ("39.4", Rgb 3 4 3)
    , ("39.5", Rgb 3 4 3)
    , ("39.6", Rgb 3 4 3)
    , ("39.7", Rgb 3 4 3)
    , ("39.8", Rgb 3 4 3)
    , ("39.9", Rgb 3 4 3)
    , ("40.0", Rgb 3 4 3)
    ]