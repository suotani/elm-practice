module Main exposing (..)

import Browser
import Html exposing (..)
import Html.Events exposing (..)
import Html.Attributes exposing (..)
import Time.Date as Date exposing (Date, date)
import Json.Decode as JD

-- MAIN

main =
  Browser.element
    { init = init
    , update = update
    , subscriptions = subscriptions
    , view = view
    }


-- MODEL

type alias Event = 
  { id : Int
  , year : Int
  , month : Int
  , day : Int
  , title : String
  , date : String
  , place : String
  , explain : String
  }

type ViewStatus
  = Init
  | Modal
  | EventModal

type alias Model =
  { year : Int
  , month : Int
  , day : Int
  , currentYear : Int
  , currentMonth: Int
  , currentDay: Int
  , events : List Event
  , viewStatus : ViewStatus
  , eventDay : Day
  , title : String
  , date : String
  , place : String
  , explain : String
  , eventId : Int
  }

-- init : (Int, Int, Int) -> (Model, Cmd Msg)
-- init (y, m, d) =
--   ( Model y m d y m Init, Cmd.none)

init : () -> (Model, Cmd Msg)
init _ =
  ( Model 2020 2 8 2020 2 8 [] Init (0, 0, 0) "" "" "" "" 0, Cmd.none)

-- UPDATE
type Msg
  = ShowModel Day
  | ShowEventModal Event
  | CloseModal
  | InputEvent String String
  | SaveEvent
  | ChangeMonth Int
  | RemoveEvent

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    ShowModel d ->
      ( {model | viewStatus = Modal, eventDay = d}, Cmd.none)
    
    ShowEventModal e ->
      ({ model | 
            viewStatus = EventModal
          , title = e.title
          , eventDay = (e.year, e.month, e.day)
          , place = e.place
          , explain = e.explain
          , eventId = e.id
       }
      , Cmd.none)
    
    CloseModal ->
      ( {model | viewStatus = Init, title = "", date = "", place = "", explain = ""}, Cmd.none)
    
    RemoveEvent ->
      ( {model |
           events = List.filter (\e -> not (e.id == model.eventId )) model.events
         , viewStatus = Init
        }
      , Cmd.none
      )
    
    InputEvent column value ->
      case column of
        "title" ->
          ( {model | title = value}, Cmd.none)
        "date" ->
          ( {model | date = value}, Cmd.none)
        "place" ->
          ( {model | place = value}, Cmd.none)
        "explain" ->
          ( {model | explain = value}, Cmd.none)
        _ ->
          (model, Cmd.none)
    
    SaveEvent ->
      let
        (y, m, d) = model.eventDay
        headE = Maybe.withDefault (Event 0 0 0 0 "" "" "" "") <| List.head model.events
        e = Event 
              (headE.id + 1)
              y
              m
              d
              model.title
              model.date
              model.place
              model.explain
      in
      ({ model| events = e :: model.events, title = "", date = "", place = "", explain = "", viewStatus = Init }
      , Cmd.none
      )
    
    ChangeMonth diff ->
      let
        current = date model.year model.month model.day
        moved = Date.addMonths diff current
        (y, m, d) = dateToTuple moved 
      in
      ({model | year = y, month = m, day = d}, Cmd.none)

--SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions _ = 
  Sub.none

-- VIEW

view : Model -> Html Msg
view model = 
  div []
    [ div [ class "header" ]
        [ i [ class "material-icons" ] [ text "menu" ]
        , i [ class "material-icons calender-icon" ] [ text "event_note" ]
        , span [] [ text "カレンダー" ]
        , div [ class "chevrons" ]
            [ i [ class "material-icons", onClick <| ChangeMonth -1 ] [ text "chevron_left" ]
            , i [ class "material-icons", onClick <| ChangeMonth 1 ] [ text "chevron_right" ]
            ]
        , span []
          [ text <| String.fromInt model.year ++ "年" ++ String.fromInt model.month ++ "月" ]
        ]
    , div [ class "calender-main" ]
        [ div [ class "calender-header" ]
            ( List.map (\w -> div [ class "header-item" ] [ text w ]) ["日", "月", "火", "水", "木", "金", "土"] )
        , div [ class "calender-days" ] (dayList model)
        ]
    , case model.viewStatus of
        Modal ->
          div [ class "modal-back" ]
            [ div [ class "modal" ]
              [ div [ class "modal-content" ]
                [ div [ class "btn-wrap" ]
                  [ span [ class "close-btn", onClick CloseModal ]  [ text "×" ] ]
                , input [ class "title", placeholder "タイトルと日時を追加", type_ "text", onInput <| InputEvent "title" ] []
                , div [ class "input-items" ]
                  [ div [ class "input-item" ]
                    [ i [ class "material-icons" ] [ text "watch_later" ]
                    , input [ placeholder "日時を追加", type_ "date", value "2020-02-05", onInput <| InputEvent "date" ] []
                    ]
                  , div [ class "input-item" ]
                    [ i [ class "material-icons" ] [ text "place" ]
                    , input [ placeholder "場所を追加", type_ "text", onInput <| InputEvent "place" ] []
                    ]
                  , div [ class "input-item" ]
                    [ i [ class "material-icons" ] [ text "notes" ]
                    , input [ placeholder "説明を追加", type_ "text", onInput <| InputEvent "explain" ] []
                    ]
                  , div [ class "btn-wrap", onClick SaveEvent ]
                    [ button [ type_ "button" ] [ text "保存" ] ]
                  ]
                ]
              ]
            ]
        
        EventModal ->
          div [ class "modal-back" ]
            [ div [ class "modal" ]
              [ div [ class "modal-content" ]
                [ div [ class "btn-wrap" ]
                  [ span [ class "close-btn", onClick RemoveEvent ]  [ text "削除" ]
                  , span [ class "close-btn", onClick CloseModal ]  [ text "×" ]
                  ]
                , p [] [ text model.title ]
                , div [ class "input-items" ]
                  [ if not (model.place == "") then
                      div [ class "input-item" ]
                      [ i [ class "material-icons" ] [ text "place" ]
                      , span [] [ text model.place ]
                      ]
                    else
                      text ""

                  , if not (model.explain == "") then
                      div [ class "input-item" ]
                      [ i [ class "material-icons" ] [ text "notes" ]
                      , span [] [text model.explain]
                      ]
                    else
                      text ""
                  ]
                ]
              ]
            ]

        Init ->
          text ""
    ]

dayList : Model -> List (Html Msg)
dayList model =
  List.map (\d -> viewDay model d) <| createDates model.year model.month

viewDay : Model -> Day -> Html Msg
viewDay model d =
  div [ class "day", onClick <| ShowModel d ]
    [ p [ class "date" ] [ text <| toStringDay d ]
    , div [ class "day-events" ] ( List.map (\e -> viewEventTag e )  <| findEvents model d )
    ]

toStringDay : Day -> String
toStringDay (_, _, d) = String.fromInt d

viewEventTag : Event -> Html Msg
viewEventTag e =
  p [ class "event", customOnClick <| ShowEventModal e ] [ text e.title ]

customOnClick : msg -> Attribute msg
customOnClick msg =
  Html.Events.custom "click" (JD.succeed { message = msg, stopPropagation = True, preventDefault = True})

findEvents : Model -> Day -> List Event
findEvents model (y, m, d) =
  List.filter (\e -> e.year == y && e.month == m && e.day == d) model.events

type alias Day = (Int, Int, Int)


-- DATA

toIntWeekday : Date.Weekday -> Int
toIntWeekday d =
  case d of
    Date.Sun -> 0
    Date.Mon -> 1
    Date.Tue -> 2
    Date.Wed -> 3
    Date.Thu -> 4
    Date.Fri -> 5
    Date.Sat -> 6

lastDay : Int -> Int -> Int
lastDay y m = Date.daysInMonth y m

createDates : Int -> Int -> List Day
createDates year month =
  let
    left = leftDates (year, month, 1)
    middle = List.map (\d -> (year, month, d)) <| List.range 1 <| lastDay year month
    right = rightDates (year, month, (lastDay year month))
  in
  left ++ middle ++ right

leftDates : Day -> List Day
leftDates (y , m, d) =
  List.map
    (\diff -> dateToTuple <| (date y m d |> Date.addDays diff))
    <| List.range -(toIntWeekday <| Date.weekday <| date y m d) -1

rightDates : Day -> List Day
rightDates (y, m, d) =
  List.map
    (\diff -> dateToTuple <| (date y m d |> Date.addDays diff))
    <| List.range 1 (6 - (toIntWeekday <| Date.weekday <| date y m d))

dateToTuple : Date -> Day
dateToTuple d =
  (Date.year d, Date.month d, Date.day d)