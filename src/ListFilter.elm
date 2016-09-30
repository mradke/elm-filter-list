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
    { init = \flags -> (flags, fetchData flags.url)
    , view = view
    , update = update
    , subscriptions = subscriptions
    }

-- MODEL

type alias HerbDescription =
  { botanicalname : String
  , productname : String
  }

type alias Model =
  { herbs : List HerbDescription
  , filterBy : String
  , message : String
  , url : String
  }

-- UPDATE

type Msg
  = NoOp
  | NewFilter String
  | Init Model
  | FetchData
  | ErrorOccurred String
  | DataFetched (List HerbDescription)

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    NoOp ->
      (model, Cmd.none)

    Init initModel ->
      (initModel, Cmd.none)

    -- sets a new filter value onInput from the view
    NewFilter inputValue ->
      let newModel = { model | filterBy = inputValue }
      in
        (newModel, Cmd.none)
    
    -- Handle https get requests
    FetchData ->
      let newModel = { model | message = "" }
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

subscriptions : Model -> Sub Msg
subscriptions model = Sub.none

-- VIEW

view : Model -> Html Msg
view model =
  let
    herbs =
      List.map viewHerbDescription
        <| List.filterMap (removeUnmatched model.filterBy) model.herbs
  in
    div []
      [ showMessage model.message
      , input [ class "filter-box", placeholder "Filter", onInput NewFilter ] []
      , div [ class "herblist-header" ]
        [ span [] [ text "Product name" ]
        , span [] [ text "Botanical name" ]
        ]
      , div [class "herblist"] herbs
      ]

showMessage : String -> Html Msg
showMessage message =
  if String.isEmpty message then
    text ""
  else
    div [class "herb-message"] [text message]    

removeUnmatched : String -> HerbDescription -> Maybe HerbDescription
removeUnmatched fltr desc =
  let
      botanicalname = String.toLower desc.botanicalname
      productname = String.toLower desc.productname
      filter = String.toLower fltr
  in
    if contains filter botanicalname then
      Just desc
    else if contains filter productname then
      Just desc
    else
      Nothing

viewHerbDescription : HerbDescription -> Html Msg
viewHerbDescription model =
  div [class "list-item"]
    [ span [] [ text model.botanicalname ]
    , span [] [ text model.productname ]
    ]

-- HTTP CALL AND DECODERS

herbDateDecoder : Json.Decoder HerbDescription
herbDateDecoder =
  Json.object2
    HerbDescription
    ("productname" := string)
    ("botanicalname" := string)

herbListDecoder : Json.Decoder (List HerbDescription)
herbListDecoder =
  Json.list herbDateDecoder

fetchData : String -> Cmd Msg
fetchData url =
  Http.get herbListDecoder url
    |> Task.mapError toString
    |> Task.perform ErrorOccurred DataFetched
