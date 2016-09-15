import Html exposing (Html, button, div, text)
import Html.App as App
import Html.Events exposing (onClick)

main =
  App.beginnerProgram { model = 0, view = view, update = update }

-- MODEL
type alias Position =
    { lat : Float
    , lng : Float
    }

type alias Model = Position

init : Model
init = { lat = 43, lng = 4.5 }

-- UPDATE
type Action =
      North

update : Action -> Model -> (Model, Effects Action)
update action model =
    case action of
        North -> ( { model = lat <- model.lat + 1 }, Effects.none )

-- VIEW
view : Signal.Address Action -> Model -> Html
view address model =
    div []
        [ button [ onClick address North ] [ text "North" ]
        ]

-- Main Elm Architecture port
port tasks : Signal (Task.Task Never ())
port tasks =
  app.tasks

-- extra port
port lat : Signal Float
port lat = Signal.map (\m -> m.lat) app.model
