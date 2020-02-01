import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import List

-- MAIN

main =
  Browser.element
    { init = init
    , update = update
    , subscriptions = subscriptions
    , view = view
    }


-- MODEL
type alias Step = 
  { urls : String
  , text : String
  , id : Int
  }

type alias Model =
  { title : String
  , explain : String
  , steps : List Step 
  }

init : () -> (Model, Cmd Msg)
init _ =
  (Model "" "" [Step "" "" 1], Cmd.none)


-- UPDATE
type Msg
  = AddField
  | DeleteField Int
  | Input String Int String

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    AddField ->
      ({model | steps = List.append model.steps [Step "" "" (List.length model.steps + 1)]}
      , Cmd.none)
    
    DeleteField id ->
      ({ model | steps = reNumbering (deleteListFromID id model.steps) }, Cmd.none)
    
    Input column id val ->
      ({model | steps = fieldUpdater model.steps id column val }, Cmd.none)

deleteListFromID : Int -> List Step -> List Step
deleteListFromID id steps = List.filter (\x -> not(x.id == id)) steps

reNumbering : List Step -> List Step
reNumbering steps =
  List.indexedMap (\i step -> Step step.urls step.text (i+1)) steps

fieldUpdater : List Step -> Int -> String -> String -> List Step
fieldUpdater steps id column val = List.map (updateField id column val) steps

updateField : Int -> String -> String -> Step -> Step
updateField id column val step =
  if step.id == id then
    case column of
      "urls" ->
        Step val step.text id
      
      "text" ->
        Step step.urls val id
      
      _ ->
        step
  else
    step

--SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model = 
  Sub.none

-- VIEW

view : Model -> Html Msg
view model = 
  div []
    [ input [type_ "text", placeholder "title"] [] 
    , input [type_ "explain", placeholder "explain"] [] 
    , button [onClick AddField] [text "append"]
    , div []
        (List.map fieldView model.steps)
    ]

fieldView : Step -> Html Msg
fieldView step =
  div []
    [ p []
        [text (String.fromInt step.id)
        ,button [onClick (DeleteField step.id)] [text ("delete" ++ (String.fromInt step.id))]
        ]
    , div []
        [ input
            [ type_ "text"
            , placeholder "urls"
            , value step.urls
            , name ("curriculum[steps_attributes][" ++ (String.fromInt step.id) ++ "][urls]") 
            , onInput (Input "urls" step.id)
            ] []]
    , div []
        [ input
           [ type_ "text"
           , placeholder "text"
           , value step.text
           , name ("curriculum[steps_attributes][" ++ (String.fromInt step.id) ++ "][text]")
           , onInput (Input "text" step.id)
          ] []]
    ]