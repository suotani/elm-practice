import Browser
import Html exposing (..)
import Html.Events exposing (..)
import Html.Attributes exposing (..)
import Http
import Json.Decode as D exposing (Decoder)

-- MAIN

main =
  Browser.element
    { init = init
    , update = update
    , subscriptions = \_ -> Sub.none
    , view = view
    }


-- MODEL
type alias Model =
  { input : String
  , viewStatus : ViewStatus
  }

type ViewStatus
  = Init
  | Waiting
  | Loaded User
  | Failed Http.Error

init : () -> (Model, Cmd Msg)
init _ =
  ( Model "" Init
  , Cmd.none
  )


-- UPDATE
type Msg
  = Input String
  | Send
  | Recieve (Result Http.Error User)

update : Msg -> Model -> (Model, Cmd Msg)
update msg model = 
  case msg of
    Input newInput ->
      ( { model | input = newInput }, Cmd.none)
    
    Send ->
      ( { model 
          | input = ""
          , viewStatus = Waiting
        }
      , Http.get
          { url = "https://api.github.com/users/" ++ model.input
          , expect = Http.expectJson Recieve userDecorder
          }
      )
    
    Recieve (Ok user) ->
      ( { model | viewStatus = Loaded user}, Cmd.none )
    
    Recieve (Err e) ->
      ( { model | viewStatus = Failed e} , Cmd.none )


-- VIEW

view : Model -> Html Msg
view model = 
  div []
    [ Html.form [onSubmit Send]
        [ input 
            [ onInput Input
            , autofocus True
            , placeholder "Github name"
            , value model.input 
            ]
            []
        , button
            [ disabled ((model.viewStatus == Waiting) || String.isEmpty (String.trim model.input)) ]
            [text "Search"]
        ]
    , case model.viewStatus of
        Init ->
          text ""
        
        Waiting ->
          text "Waiting..."
        
        Loaded user ->
          a [ href user.htmlUrl, target "_blank" ]
            [ img [ src user.avatarUrl, width 200 ] []
            , div [][text (Maybe.withDefault user.login user.name)]
            , div []
                [ case user.bio of
                    Just bio ->
                      text bio
                    
                    Nothing ->
                      text ""
                ]
            ]
        
        Failed error ->
          div [] [text (Debug.toString error)]

    ]


-- DATA

type alias User =
  { login : String
  , avatarUrl : String
  , name : Maybe String
  , htmlUrl : String
  , bio : Maybe String
  }

userDecorder : Decoder User
userDecorder = 
  D.map5 User
    (D.field "login" D.string)
    (D.field "avatar_url" D.string)
    (D.maybe (D.field "name" D.string))
    (D.field "html_url" D.string)
    (D.maybe (D.field "bio" D.string))