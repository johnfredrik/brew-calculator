module Main exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing ( onInput, onClick,on )
import Ibu as Ibu exposing(..)
import Srm exposing(..)
import Abv exposing(..)
-- APP
main : Program Never Model Msg
main =
  Html.program { init = (model, initialCmd), view = view, update = update, subscriptions = (\_ -> Sub.none) }


-- MODEL
type alias Model =
  { abv : Abv.Model
  , ibu : Ibu.Model
  , srm : Srm.Model
  , calculator : Calculator
  , error : Maybe String
  }

model : Model
model = 
    { abv = Abv.init
    , ibu = Ibu.init
    , srm = Srm.init
    , calculator = AbvCalculator
    , error = Nothing
    }

type Calculator
  = AbvCalculator
  | IbuCalculator
  | SrmCalculator

--   UPDATE
type Msg 
  = NoOp
  | ChangeCalculator Calculator
  | AbvMsg Abv.Msg
  | IbuMsg Ibu.Msg
  | SrmMsg Srm.Msg

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
    NoOp -> (model, Cmd.none)
    ChangeCalculator calculator ->
       ({ model | calculator = calculator}, Cmd.none)
    AbvMsg abvMsg ->
      updateAbv abvMsg model 
    IbuMsg ibuMsg ->
      updateIbu ibuMsg model 
    SrmMsg srmMsg ->
      updateSrm srmMsg model


updateAbv : Abv.Msg -> Model -> (Model, Cmd Msg)
updateAbv abvMsg model =
  let
    newAbv = (Abv.update abvMsg model.abv)
  in
      ( { model | abv = newAbv}, Cmd.none)

updateIbu : Ibu.Msg -> Model -> (Model, Cmd Msg)
updateIbu ibuMsg model =
  let
    (ibuModel, ibuCmd) = (Ibu.update ibuMsg model.ibu)
  in
    ({model | ibu = ibuModel}, Cmd.map IbuMsg ibuCmd)

updateSrm : Srm.Msg -> Model -> (Model, Cmd Msg)
updateSrm srmMsg model =
  let
    (srmModel, srmCmd) = (Srm.update srmMsg model.srm)
  in
    ({model | srm = srmModel}, Cmd.map SrmMsg srmCmd)

initialCmd : Cmd Msg
initialCmd = 
   Cmd.map IbuMsg Ibu.initialCmd


-- VIEW
view : Model -> Html Msg
view model =
  div 
    [ class "content"] 
    [ div [ class "header" ] 
          [ div [ classList [("selected-header", model.calculator == AbvCalculator)], onClick (ChangeCalculator AbvCalculator)] [ text "ABV" ]
          , div [ classList [("selected-header", model.calculator == IbuCalculator)], onClick (ChangeCalculator IbuCalculator)] [ text "IBU" ]
          , div [ classList [("selected-header", model.calculator == SrmCalculator)], onClick (ChangeCalculator SrmCalculator)] [ text "SRM" ]
          ]
    , div [] 
          [ (case model.calculator of
              AbvCalculator ->
                Abv.viewAbv model.abv
                  |> Html.map AbvMsg
              IbuCalculator ->
                Ibu.viewIbu model.ibu
                  |> Html.map IbuMsg
              SrmCalculator ->
                Srm.viewSrm model.srm
                  |> Html.map SrmMsg
            )
          ]
    ]