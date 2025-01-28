module Main exposing (..)
import Browser
import Html exposing (Html, div, input, text, button)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput, onClick)

-- MAIN
main =
  Browser.sandbox { init = init, update = update, view = view }

-- MODEL
type alias Model =
  { content : String
  , dessin : String
  , showDrawing : Bool
  }

init : Model
init =
  Model "" "" False  -- La fenêtre de dessin n'est pas affichée au départ

-- UPDATE
type Msg
  = Content String 
  | Dessin

update : Msg -> Model -> Model
update msg model =
  case msg of
    Content content ->
      { model | content = content }
    Dessin ->
      { model | showDrawing = True }  -- Quand on clique sur "Draw", on affiche la fenêtre de dessin

-- VIEW
view : Model -> Html Msg
view model =
  div []
    [ div [] -- Premier div
        [ text ("Type in your code below:") ]
    , div [] -- Deuxième div avec champ de saisie plus large
        [ input [ placeholder "Example : [Forward 100, Repeat 4 [Forward 50, Left 90], Forward 100]", value model.content, onInput Content, style "width" "100%" ] []
        ]
    , button [ onClick Dessin ] [ text "Draw" ]  -- Bouton pour afficher la fenêtre de dessin
    , if model.showDrawing then  -- Si `showDrawing` est `True`, on affiche la fenêtre de dessin
        div [ style "border" "1px solid black", style "width" "400px", style "height" "400px" ]
          []  -- Contenu de la fenêtre de dessin vide
      else
        text ""  -- Si `showDrawing` est `False`, rien n'est affiché
    ]
