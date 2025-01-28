module Main exposing (..)
import Browser
import Html exposing (Html, div, input, text)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput)



-- MAIN


main =
  Browser.sandbox { init = init, update = update, view = view }



-- MODEL


type alias Model =
  { content : String
  }


init : Model
init =
  { content = "" }



-- UPDATE


type Msg
  = Change String


update : Msg -> Model -> Model
update msg model =
  case msg of
    Change newContent ->
      { model | content = newContent }



-- VIEW


view : Model -> Html Msg
view model =
  div []
    [ div [] -- Premier div
        [ text ("Type in your code below:") ]
    , div [] -- Deuxième div
        [ input [ placeholder "Text to reverse", value model.content, onInput Change ] []
        , div [] [ text (String.reverse model.content) ]
        ]
    ]

  