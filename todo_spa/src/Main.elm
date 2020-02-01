module Main exposing (main)
import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http exposing (..)
import Json.Encode as Encode
import Json.Decode exposing(Decoder, field, string, int, map4, list)
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
type alias Todo =
  { id : Int
  , status : Int
  , title : String
  , explain : String
  }

type alias Model =
  { todos : List Todo
  , todo : Todo
  , display : Display
  }

init : () -> (Model, Cmd Msg)
init _ =
  ( Model [] (Todo 0 0 "" "" ) Loading
  , getTodoList
  )

-- UPDATE
type Msg
  = GotList (Result Http.Error (List Todo))
  | DisplayNew
  | CreateTodo
  | GotCreateResult (Result Http.Error String)
  | EditTodo Int
  | UpdateTodo 
  | DeleteTodo Int
  | GotDeletedResult (Result Http.Error String)
  | Title String
  | Explain String

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    GotList result ->
      case result of
        Ok todos ->
          ( { model | todos = todos, display = Index }, Cmd.none)
        
        Err _ ->
          ( model, Cmd.none )
    
    CreateTodo ->
      ( model, createTodo model )

    GotCreateResult result ->
      case result of
        Ok _ ->
          ( { model | todo = Todo 0 0 "" "", display = Loading }, getTodoList)
        
        Err _ ->
          ( model, Cmd.none )
    
    DeleteTodo id ->
      (model, deleteTodo model id)
    
    GotDeletedResult result ->
      case result of
        Ok _ ->
          ( model, getTodoList)
        
        Err _ ->
          ( model, Cmd.none )
    
    EditTodo id ->
      ({ model | display = Edit, todo = Maybe.withDefault (Todo 0 0 "" "") (List.head (List.filter (\x -> x.id == id) model.todos))}
      , Cmd.none
      )
    
    UpdateTodo ->
      (model, updateTodo model)
    
    DisplayNew ->
      ( { model | display = New }, Cmd.none)
    
    Title title ->
      ( { model | todo = Todo model.todo.id model.todo.status title model.todo.explain }, Cmd.none)

    Explain explain ->
      ( { model | todo = Todo model.todo.id model.todo.status model.todo.title explain }, Cmd.none)

--SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions _ = 
  Sub.none


-- VIEW

type Display
  = Index
  | Loading
  | New
  | Edit


view : Model -> Html Msg
view model = 
  case model.display of
    Index ->
      indexView model
    
    Loading ->
      loadingView
    
    New ->
      newView model
    
    Edit ->
      editView model

loadingView : Html Msg
loadingView =
  div []
    [ text "Loading..." ]

indexView : Model -> Html Msg
indexView model = 
  div []
    [ button [ onClick DisplayNew] [ text "New Item" ]
    , div [] (List.map todoListItem model.todos )
    ]


todoListItem : Todo -> Html Msg
todoListItem todo =
  div []
    [ p []
        [ text ("(" ++ String.fromInt todo.id ++ "):" ++ String.fromInt todo.status ++ ":" ++ todo.title)
        , button [onClick (DeleteTodo todo.id)] [text ("Delete! ->" ++ String.fromInt todo.id)]
        , button [onClick (EditTodo todo.id)] [text ("Edit! ->" ++ String.fromInt todo.id)]
        ]
    , p [] [ text ("->" ++ todo.explain) ]
    ]

newView : Model -> Html Msg
newView model =
  div []
    [ div []
        [ input [ type_ "text", placeholder "input title" ,onInput Title ] [] ]
    , div []
        [ textarea [ placeholder "input explain" ,onInput Explain ] [] ]
    , div []
        [ text (String.fromInt model.todo.status ++ "/" ++ model.todo.title ++ ":" ++ model.todo.explain) ]
    , div []
        [ button [onClick CreateTodo ] [text "Submit!"] ]
    ]

editView : Model -> Html Msg
editView model =
  div []
    [ div []
        [ input [ type_ "text", placeholder "input title" ,value model.todo.title , onInput Title ] [] ]
    , div []
        [ textarea [ placeholder "input explain", value model.todo.explain ,onInput Explain ] [] ]
    , div []
        [ text (String.fromInt model.todo.status ++ "/" ++ model.todo.title ++ ":" ++ model.todo.explain) ]
    , div []
        [ button [onClick UpdateTodo ] [text "Submit!"] ]
    ]

-- HTTP

targetDomain : String
targetDomain = "http://localhost:3000"

getTodoList : Cmd Msg
getTodoList =
  Http.get
    { url = targetDomain ++ "/todos"
    , expect = Http.expectJson GotList listDecoder
    }

createTodo : Model -> Cmd Msg
createTodo model = 
  Http.post
    { url = targetDomain ++ "/todos"
    , body = Http.jsonBody (newTodoEncoder model.todo)
    , expect = Http.expectJson GotCreateResult simpleResultDecoder
    }

updateTodo : Model -> Cmd Msg
updateTodo model = 
  Http.request
    { method = "PATCH"
    , headers = []
    , url = targetDomain ++ "/todos/" ++ String.fromInt model.todo.id
    , body = Http.jsonBody (updateTodoEncoder model.todo)
    , expect = Http.expectJson GotCreateResult simpleResultDecoder
    , timeout = Nothing
    , tracker = Nothing
    }

deleteTodo : Model -> Int -> Cmd Msg
deleteTodo _ id =
  Http.request
    { method = "DELETE"
    , headers = []
    , url = targetDomain ++ "/todos/" ++ String.fromInt id
    , body = Http.emptyBody
    , expect = Http.expectJson GotDeletedResult simpleResultDecoder
    , timeout = Nothing
    , tracker = Nothing
    }
-- JSON

listDecoder : Decoder (List Todo)
listDecoder = list todoDecoder

todoDecoder : Decoder Todo
todoDecoder = 
  map4 Todo
    (field "id" int)
    (field "status" int)
    (field "title" string)
    (field "explain" string)

simpleResultDecoder : Decoder String
simpleResultDecoder = field "status" string

newTodoEncoder : Todo -> Encode.Value
newTodoEncoder todo = 
  Encode.object
    [ ( "title", Encode.string todo.title )
    , ( "explain", Encode.string todo.explain )
    ]

updateTodoEncoder : Todo -> Encode.Value
updateTodoEncoder todo = 
  Encode.object
    [ ("id", Encode.int todo.id)
    , ( "title", Encode.string todo.title )
    , ( "explain", Encode.string todo.explain )
    ]