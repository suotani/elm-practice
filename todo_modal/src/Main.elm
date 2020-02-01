import Browser
import Html exposing (..)
import Html.Events exposing (..)

-- MAIN

main =
  Browser.element
    { init = init
    , update = update
    , subscriptions = subscriptions
    , view = view
    }


-- MODEL
type alias Model = {}

init : () -> (Model, Cmd Msg)

-- UPDATE
type Msg

update : Msg -> Model -> (Model, Cmd Msg)

--SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model = 
  Sub.none


-- VIEW

view : Model -> Html Msg
view model = 
  div []
    []