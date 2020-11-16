module Tests exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Main exposing (indexAt)
import Test exposing (..)


suite : Test
suite =
    describe "indexAt - Given a list, get the value at the given index"
        [ test "works for index 0" <|
            \_ -> Expect.equal (indexAt 0 [ "hello" ]) (Just "hello")
        , test "works for end index" <|
            \_ -> Expect.equal (indexAt 4 [ 1, 2, 3, 4, 5 ]) (Just 5)
        , test "returns nothing for out-of-bounds" <|
            \_ -> Expect.equal (indexAt 100 []) Nothing
        , test "Out of bounds for OBO" <|
            \_ -> Expect.equal (indexAt 2 [ "zero", "one" ]) Nothing
        ]
