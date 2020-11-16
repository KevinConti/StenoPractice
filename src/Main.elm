module Main exposing (..)

import Browser
import Html exposing (Html, button, div, input, p, pre, text)
import Html.Attributes exposing (style, value)
import Html.Events exposing (onClick, onInput)
import Http
import Random
import Time
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
    , wordList : WordList
    , currentMode : Mode
    , completedWordCount : Int
    , remainingTime : Int
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( initModel
    , Cmd.none
    )


initModel : Model
initModel =
    { userInput = ""
    , wordList = WordList "" []
    , currentMode = NoMode
    , completedWordCount = 0
    , remainingTime = 0
    }



-- UPDATE


type Msg
    = GotRandomNumber Int
    | ModeSelected ModeRequestMsg
    | UserInput String String
    | Tick Time.Posix


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ModeSelected modeRequestMsg ->
            updateModeSelected model modeRequestMsg

        GotRandomNumber randomNum ->
            updateGotRandomNumber model randomNum

        UserInput answer input ->
            updateUserInput model answer input

        Tick _ ->
            if model.remainingTime - 1 <= 0 then
                ( { model
                    | remainingTime = 0
                    , currentMode = NoMode
                    , wordList = WordList "" []
                  }
                , Cmd.none
                )

            else
                ( { model | remainingTime = model.remainingTime - 1 }, Cmd.none )



-- This is where we assign the list for the given mode


mapModeRequestMsg : ModeRequestMsg -> Mode
mapModeRequestMsg modeRequestMsg =
    case modeRequestMsg of
        NoModeSelected ->
            NoMode

        BasicMode ->
            Basic (WordList "" oneSyllableWords)

        TimedMode innerMode ->
            Timed (mapModeRequestMsg innerMode)


updateModeSelected : Model -> ModeRequestMsg -> ( Model, Cmd Msg )
updateModeSelected model modeRequestMsg =
    case modeRequestMsg of
        TimedMode modeType ->
            ( { model
                | currentMode = modeRequestMsg |> mapModeRequestMsg
                , remainingTime = 60
                , wordList = WordList model.wordList.currentWord oneSyllableWords
                , completedWordCount = 0
              }
            , getRandomNumber model.wordList.currentList
            )

        BasicMode ->
            ( { model
                | currentMode = modeRequestMsg |> mapModeRequestMsg
                , wordList = WordList model.wordList.currentWord oneSyllableWords
                , completedWordCount = 0
              }
            , getRandomNumber model.wordList.currentList
            )

        NoModeSelected ->
            ( { model
                | currentMode = modeRequestMsg |> mapModeRequestMsg
                , wordList = WordList "" []
                , remainingTime = 0
              }
            , Cmd.none
            )


updateGotRandomNumber : Model -> Int -> ( Model, Cmd Msg )
updateGotRandomNumber model randomNum =
    let
        newWord =
            case indexAt randomNum model.wordList.currentList of
                Nothing ->
                    "Error - indexAt experienced an issue. Index: " ++ String.fromInt randomNum

                Just a ->
                    a
    in
    -- Prevent duplicate word request
    if newWord == model.wordList.currentWord then
        ( model, getRandomNumber model.wordList.currentList )

    else
        ( { model | wordList = WordList newWord model.wordList.currentList }, Cmd.none )


updateUserInput : Model -> String -> String -> ( Model, Cmd Msg )
updateUserInput model answer input =
    if String.trim answer == String.trim input then
        ( { model
            | completedWordCount = model.completedWordCount + 1
            , userInput = ""
          }
        , getRandomNumber model.wordList.currentList
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
    Random.generate GotRandomNumber (Random.int 0 (List.length list - 1))



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    -- Create a subscription that sends a Tick message every second for Timed modes
    case model.currentMode of
        Timed _ ->
            Time.every 1000 Tick

        _ ->
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
            NoMode ->
                displayModes model.completedWordCount

            _ ->
                playGame model
        ]


displayModes : Int -> Html Msg
displayModes completedWordCount =
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
            , onClick (ModeSelected (TimedMode BasicMode))
            ]
            [ "Basic Mode - 60 Seconds" |> text ]
        , if completedWordCount > 0 then
            p [] [ "You completed " ++ String.fromInt completedWordCount ++ " words!" |> text ]

          else
            div [] []
        ]


playGame : Model -> Html Msg
playGame model =
    div
        [ style "text-align" "center"
        ]
        [ p [] [ model.wordList.currentWord |> text ]
        , input
            [ onInput (UserInput model.wordList.currentWord)
            , value model.userInput
            ]
            []
        , p [] [ "Completed words: " ++ String.fromInt model.completedWordCount |> text ]
        , button [ onClick (ModeSelected NoModeSelected) ] [ "End game" |> text ]
        , if model.remainingTime /= 0 then
            p [] [ "Current time remaining: " ++ String.fromInt model.remainingTime |> text ]

          else
            p [] []
        ]
