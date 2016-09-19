port module ListFilter exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Html.App as App
import String exposing (contains)
import Json.Decode as Json exposing (..)
import Task
import Http

main : Program Model
main
  = App.programWithFlags
    { init = \flags -> (flags, fetchData)
    , view = view
    , update = update
    , subscriptions = subscriptions
    }

-- MODEL

type alias HerbDescription =
  { latin : String
  , english : String
  }

type alias Model =
  { herbs : List HerbDescription
  , filterBy : String
  , message : String
  }

-- UPDATE

type Msg
  = NoOp
  | NewFilter String
  | Init Model
  | FetchData
  | ErrorOccurred String
  | DataFetched (List HerbDescription)

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
    
    FetchData ->
      let newModel = { model | message = "Initiating fetch" }
      in
        (newModel, Cmd.none)

    ErrorOccurred errorMessage ->
      let newModel = { model | message = "Error: " ++ errorMessage }
      in
        (newModel, Cmd.none)

    DataFetched newHerbs ->
      let newModel = { model | herbs = newHerbs }
      in
        (newModel, Cmd.none)

-- SUBSCRIPTIONS

port getInitValue : (Model -> msg) -> Sub msg

subscriptions : Model -> Sub Msg
subscriptions model = Sub.none


-- VIEW

view : Model -> Html Msg
view model =
  let
    herbs =
      List.map viewHerbDescription
        <| List.filterMap (removeUnmatched <| String.toLower model.filterBy) model.herbs
  in
    div []
      [ input [ placeholder "Filter", onInput NewFilter ] []
      , div [] herbs
      ]

removeUnmatched : String -> HerbDescription -> Maybe HerbDescription
removeUnmatched fltr desc =
  let
      latin = String.toLower desc.latin
      english = String.toLower desc.english
  in
    if contains fltr latin then
      Just desc
    else if contains fltr english then
      Just desc
    else
      Nothing

viewHerbDescription : HerbDescription -> Html Msg
viewHerbDescription model =
  div []
    [ text (model.latin ++ " - ")
    , text model.english
    ]

-- HTTP CALL AND DECODERS

herbDateDecoder : Json.Decoder HerbDescription
herbDateDecoder =
  Json.object2
    HerbDescription
    ("english" := string)
    ("latin" := string)

herbListDecoder : Json.Decoder (List HerbDescription)
herbListDecoder =
  Json.list herbDateDecoder

fetchData : Cmd Msg
fetchData =
  Http.get herbListDecoder "http://0.0.0.0:3000/herbs"
    |> Task.mapError toString
    |> Task.perform ErrorOccurred DataFetched
