port module ListFilter exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Html.App as App
import String exposing (contains)

main : Program Model
main
  = App.programWithFlags
    { init = \flags -> (flags, Cmd.none)
    , view = view
    , update = update
    , subscriptions = subscriptions
    }

-- MODEL

type alias HerbDescription =
  { latin : String
  , german : String
  , english : String
  }

type alias Model =
  { herbs : List HerbDescription
  , filterBy : String
  }

init : (Model, Cmd Msg)
init = ({herbs = [], filterBy = ""}, Cmd.none)


-- UPDATE

type Msg
  = NoOp
  | NewFilter String
  | Init Model

port syncmap : Model -> Cmd msg

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    NoOp ->
      (model, Cmd.none)
    NewFilter inputValue ->
      let newModel = { model | filterBy = inputValue }
      in
        (newModel, Cmd.none)
    Init initModel ->
      (initModel, Cmd.none)


-- SUBSCRIPTIONS

port getInitValue : (Model -> msg) -> Sub msg

subscriptions : Model -> Sub Msg
subscriptions model = Sub.none


-- VIEW

view : Model -> Html Msg
view model =
  let
    herbs =
      List.map viewHerbDescription (List.filterMap (removeUnmatched model.filterBy) model.herbs)
  in
    div []
      [ input [ placeholder "Filter latin", onInput NewFilter ] []
      , div [] herbs
      ]

removeUnmatched : String -> HerbDescription -> Maybe HerbDescription
removeUnmatched fltr desc =
  if contains fltr desc.latin then
    Just desc

  else
    Nothing

viewHerbDescription : HerbDescription -> Html Msg
viewHerbDescription model =
  div []
    [ text (model.latin ++ " - ")
    , text (model.german ++ " - ")
    , text model.english
    ]
