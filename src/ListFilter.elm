port module ListFilter exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Html.App as App
import Array exposing (Array)
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
  { latin : String
  , english : String
  }

type alias Model =
  { herbs : Array HerbDescription
  , filterBy : String
  , message : String
  , url : String
  , offset : Int
  }

-- UPDATE

type Msg
  = NoOp
  | NewFilter String
  | Init Model
  | FetchData
  | ErrorOccurred String
  | DataFetched (Array HerbDescription)
  | Offset Int

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

    Offset newOffset ->
      let newModel = { model | offset = newOffset }
      in
        ( newModel, Cmd.none )

-- SUBSCRIPTIONS

port offset : ( Int -> msg ) -> Sub msg

subscriptions : Model -> Sub Msg
subscriptions model =
  offset Offset

-- VIEW

view : Model -> Html Msg
view model =
  let
    index = (model.offset // 80)
    herbs = Array.indexedMap viewHerbDescription
      <| Array.fromList 
      <| List.filterMap (removeUnmatched <| String.toLower model.filterBy) 
      <| Array.toList model.herbs

    start = \index ->
      if (index - 100) > 0 then
        index - 100
      else
        0

    end = \index ->
      if (index + 100) < ((Array.length herbs) - 1)  then
        index + 100
      else
        (Array.length herbs) - 1

    viewable = Array.slice (start index) (end index) herbs
  in
    div []
      [ text model.message
      , text (toString model.offset)
      , text (" Index: " ++ (toString index))
      , input [ class "filter-box", placeholder "Filter", onInput NewFilter ] []
      , div [ class "herblist-header" ] 
        [ span [] [ text "Product name" ]
        , span [] [ text "Botanical name" ]
        ]
      , div 
        [ class "herblist-content" ] 
        [ div 
          [ class "herblist"
          , style 
            [ ("min-height", (toString ((Array.length model.herbs) * 80)) ++ "px")
            , ("position", "relative")
            ]
          ] 
          (Array.toList viewable)
        ] 
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

viewHerbDescription : Int -> HerbDescription -> Html Msg
viewHerbDescription position model =
  div 
    [ class "list-item"
    , style 
      [ ("top", ((toString (position * 80)) ++ "px"))
      , ("position", "absolute") 
      ]
    ]
    [ span [] [ text (toString position), text model.latin ]
    , span [] [ text model.english ]
    ]

-- HTTP CALL AND DECODERS

herbDateDecoder : Json.Decoder HerbDescription
herbDateDecoder =
  Json.object2
    HerbDescription
    ("english" := string)
    ("latin" := string)

herbListDecoder : Json.Decoder (Array HerbDescription)
herbListDecoder =
  Json.array herbDateDecoder

fetchData : String -> Cmd Msg
fetchData url =
  Http.get herbListDecoder url
    |> Task.mapError toString
    |> Task.perform ErrorOccurred DataFetched
