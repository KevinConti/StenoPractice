-- Make a GET request to load a book called "Public Opinion"
--
-- Read how it works:
--   https://guide.elm-lang.org/effects/http.html
--


module Main exposing (..)

import Browser
import Html exposing (Html, button, div, input, p, pre, text)
import Html.Attributes exposing (style, value)
import Html.Events exposing (onClick, onInput)
import Http
import Random
import Types exposing (..)
import Words exposing (oneSyllableWords)



-- MAIN


main =
    Browser.document
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }



-- MODEL


type alias Model =
    { userInput : String
    , currentWord : String
    , currentList : List String
    , currentMode : Mode
    , completedWordCount : Int
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( initModel
    , Cmd.none
    )


initModel : Model
initModel =
    { userInput = ""
    , currentWord = ""
    , currentList = []
    , currentMode = NoModeSelected
    , completedWordCount = 0
    }



-- UPDATE


type Msg
    = GotRandomNumber Int
    | ModeSelected Mode
    | UserInput String String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ModeSelected mode ->
            ( { model
                | currentMode = mode
                , currentList = oneSyllableWords
              }
            , getRandomNumber model.currentList
            )

        GotRandomNumber randomNum ->
            let
                newWord =
                    case indexAt randomNum model.currentList of
                        Nothing ->
                            "Error - indexAt experienced an issue"

                        Just a ->
                            a
            in
            -- Prevent duplicate word request
            if newWord == model.currentWord then
                ( model, getRandomNumber model.currentList )

            else
                ( { model | currentWord = newWord }, Cmd.none )

        UserInput answer input ->
            if String.trim answer == String.trim input then
                ( { model
                    | completedWordCount = model.completedWordCount + 1
                    , userInput = ""
                  }
                , getRandomNumber model.currentList
                )

            else
                ( { model | userInput = input }, Cmd.none )


indexAt : Int -> List a -> Maybe a
indexAt index list =
    if List.length list >= index then
        List.take index list
            -- [ 1, 2, 3 ]
            |> List.reverse
            -- [ 3, 2, 1 ]
            |> List.head
        -- Just 3

    else
        Nothing



-- Defaults to singleSyllableWords


getRandomNumber : List String -> Cmd Msg
getRandomNumber maybeList =
    let
        list =
            if List.isEmpty maybeList then
                oneSyllableWords

            else
                maybeList
    in
    Random.generate GotRandomNumber (Random.int 0 (List.length list))



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


type alias Document msg =
    { title : String
    , body : List (Html msg)
    }


view : Model -> Document Msg
view model =
    Document "Steno Practice"
        [ case model.currentMode of
            NoModeSelected ->
                displayModes

            BasicMode ->
                playGame model.currentWord model.userInput model.completedWordCount
        ]


displayModes : Html Msg
displayModes =
    div
        [ style "text-align" "center"
        ]
        [ p []
            [ "Select a mode" |> text ]
        , button
            [ style "padding" "4px"
            , onClick (ModeSelected BasicMode)
            ]
            [ "Basic Mode" |> text ]
        , button
            [ style "padding" "4px"
            , style "margin-top" "4px"
            ]
            [ "Basic Mode - 60 Seconds" |> text ]
        ]


playGame : String -> String -> Int -> Html Msg
playGame currentWord userInput completedWordCount =
    div
        [ style "text-align" "center"
        ]
        [ p [] [ currentWord |> text ]
        , input
            [ onInput (UserInput currentWord)
            , value userInput
            ]
            []
        , p [] [ "Completed words: " ++ String.fromInt completedWordCount |> text ]
        , button [ onClick (ModeSelected NoModeSelected) ] [ "End game" |> text ]
        ]
