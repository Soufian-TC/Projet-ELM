module Main exposing (..)

import Browser
import Html exposing (Html, button, div, input, text)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)


-- MAIN

main =
    Browser.sandbox { init = init, update = update, view = view }


-- MODEL

type alias Model =
    { content : String
    , instructions : List (String, Int) -- Liste des instructions avec leur répétition
    }


init : Model
init =
    { content = ""
    , instructions = []
    }


-- UPDATE

type Msg
    = UpdateInput String -- Met à jour le contenu tapé
    | ParseInstructions -- Parse les instructions après validation

update : Msg -> Model -> Model
update msg model =
    case msg of
        UpdateInput newContent ->
            { model | content = newContent }

        ParseInstructions ->
            let
                parsedInstructions = parseInstructions model.content
            in
            { model | instructions = parsedInstructions }


parseInstructions : String -> List (String, Int)
parseInstructions input =
    let
        -- Fonction pour couper la chaîne d'entrée en tokens
        tokenize : String -> List String
        tokenize str =
            String.split " " str -- On sépare les mots par des espaces

        -- Fonction pour parser une instruction imbriquée
        parseInstruction : List String -> List (String, Int)
        parseInstruction tokens =
            case tokens of
                -- Cas de crochet ouvrant, début d'un bloc
                "[" :: rest -> parseInstruction rest
          

                -- Cas de crochet fermant, fin du bloc
                "]" :: rest -> 
                    -- Ici, on renvoie une liste vide pour marquer la fin du bloc
                    [] 

                -- Cas de l'instruction Forward
                "Forward" :: value :: rest ->
                    case String.toInt value of
                        Just n -> [("Forward", n)] ++ parseInstruction rest
                        Nothing -> parseInstruction rest

                -- Cas de l'instruction Right
                "Right" :: value :: rest ->
                    case String.toInt value of
                        Just n -> [("Right", n)] ++ parseInstruction rest
                        Nothing -> parseInstruction rest

                -- Cas de l'instruction Left
                "Left" :: value :: rest ->
                    case String.toInt value of
                        Just n -> [("Left", n)] ++ parseInstruction rest
                        Nothing -> parseInstruction rest

                -- Cas du Repeat
                "Repeat" :: nStr :: "[" :: rest ->
                    case String.toInt nStr of
                        Just n ->
                            let
                                -- Récupérer les instructions dans les crochets
                                (instructionsInside, restAfterRepeat) = parseBlock rest
                            in
                            -- Répéter les instructions imbriquées 'n' fois, puis continuer après le Repeat
                            List.concatMap (\_ -> instructionsInside) (List.repeat (n-1) ()) ++ parseInstruction restAfterRepeat
                        Nothing -> parseInstruction rest

                -- Si on rencontre une virgule, on l'ignore et on continue avec les tokens suivants
                "," :: rest -> parseInstruction rest

                -- Sinon, on termine ou on passe à la prochaine instruction
                _ -> []

        -- Fonction pour parser un bloc de code entre crochets
        parseBlock : List String -> (List (String, Int), List String)
        parseBlock tokens =
            case tokens of
                -- Si on rencontre un crochet fermant, c'est la fin du bloc
                "]" :: rest -> ([], rest)

                -- Sinon, on garde les instructions et on continue
                _ ->
                    -- À ce point, parseInstruction renvoie déjà une liste de tuples
                    let
                        -- Pour traiter correctement les instructions imbriquées
                        innerInstructions = parseInstruction tokens
                    in
                    (innerInstructions, tokens)

    in
    -- Tokeniser l'entrée et commencer à analyser
    tokenize input
        |> parseInstruction




-- VIEW

view : Model -> Html Msg
view model =
    div []
        [ div []
            [ text "Enter your instructions:" ]
        , div []
            [ input
                [ placeholder "Enter instructions"
                , value model.content
                , onInput UpdateInput
                ]
                []
            , button
                [ onClick ParseInstructions ]
                [ text "Parse Instructions" ]
            ]
        , div []
            [ text "Parsed Instructions: " ]
        , div []
            [ text (String.join ", " (List.map (\(cmd, count) -> cmd ++ " " ++ String.fromInt count) model.instructions)) ]
        ]
