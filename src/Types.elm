module Types exposing (..)

-- A set of requests to set a mode. The reason this is its own type compared to Mode
-- is so that we separate the idea of a request to create a new Mode (a request does not have a WordList)
-- Compared to an actual mode, which might have a WordList depending on the sub-type
-- This way, we are able to couple the existence of a WordList to an existing Mode


type ModeRequestMsg
    = NoModeSelected
    | BasicMode
    | TimedMode ModeRequestMsg


type Mode
    = NoMode
    | Basic WordList
    | Timed Mode



-- Function that handles the recursive case and handles applying the function correctly


mapModes : Mode -> (WordList -> b) -> b -> b
mapModes mode func default =
    case mode of
        NoMode ->
            default

        Basic wordList ->
            func wordList

        Timed innerMode ->
            case innerMode of
                Basic wordList ->
                    func wordList

                _ ->
                    default


type alias WordList =
    { currentWord : String
    , currentList : List String
    }
