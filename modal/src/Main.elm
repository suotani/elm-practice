module Main exposing (main)
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
type alias Model =
  { modal: Bool }

init : () -> (Model, Cmd Msg)
init _ =
  ( Model False
  , Cmd.none
  )

-- UPDATE
type Msg
  = Open
  | Close

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Open ->
      ( Model True
      , Cmd.none
      )
    
    Close ->
      ( Model False
      , Cmd.none
      )
--SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model = 
  Sub.none

-- VIEW

view : Model -> Html Msg
view model =
  if model.modal then
    div []
      [ div [] [ text "modal opened" ]
      , button [ onClick Close ] [ text "close modal" ]
      ]
  else
    div []
      [ button [ onClick Open] [ text "open" ]
      ]