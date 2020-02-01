import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput, onClick)

-- MAIN

main = 
  Browser.sandbox { init = init, update = update, view = view}


-- MODEL
type alias Model = 
  { name : Maybe String
  , password : String
  , passwordAgain : String
  }

init : Model
init = 
  Model Nothing "" ""

-- UPDATE

type Msg
  = Name String
  | Password String
  | PasswordAgain String
  | Login

update : Msg -> Model -> Model
update msg model = 
  case msg of
    Name name ->
      if name == "" then
        {model | name = Nothing}
      else
        {model | name = Just name}
    
    Password password ->
      {model | password = password}
    
    PasswordAgain pwa ->
      {model | passwordAgain = pwa}
    
    Login ->
      model
         
      
    
-- VIEW

view : Model -> Html Msg
view model = 
  div []
    [ viewInput "text" "Name" (Maybe.withDefault "" model.name) Name
    , viewInput "password" "Password" model.password Password
    , viewInput "password" "Re-enter Password" model.passwordAgain PasswordAgain
    , viewValidation model
    , div [] 
        [ loginButton model

        ]
    ]

viewInput : String -> String -> String -> (String -> msg) -> Html msg
viewInput t p v toMsg = 
  input [ type_ t, placeholder p, value v, onInput toMsg ] []

viewValidation : Model -> Html msg
viewValidation model = 
  if model.password == model.passwordAgain then
    div [ style "color" "green" ] [ text "ok" ]
  else
    div [ style "color" "red" ] [ text "passwords do not match!" ]

loginButton : Model -> Html Msg
loginButton model =
  case model.name of
    Nothing ->
      div [ ] [ text "no-login" ]
    
    Just name ->
      button [ onClick Login ] [ text "login" ]