module Tests exposing (all)

import AES exposing (..)
import AES.Types exposing (..)
import AES.Utility exposing (..)
import Array exposing (Array, fromList)
import BitwiseInfix exposing (..)
import Expect exposing (Expectation)
import List
import Maybe exposing (withDefault)
import Test exposing (..)


log =
    Debug.log


{-| change to True to log JSON input & output results
-}
enableLogging : Bool
enableLogging =
    False


maybeLog : String -> a -> a
maybeLog label value =
    if enableLogging then
        log label value
    else
        value


testMap : (x -> String -> Test) -> List x -> List Test
testMap test data =
    let
        numbers =
            List.map toString <| List.range 1 (List.length data)
    in
    List.map2 test data numbers


all : Test
all =
    Test.concat <|
        List.concat
            [ List.map doTest intData
            , List.map doTest arrayData
            ]


expectResult : Result err a -> Result err a -> Expectation
expectResult sb was =
    case maybeLog "  result" was of
        Err err ->
            case sb of
                Err _ ->
                    Expect.true "You shouldn't ever see this." True

                Ok _ ->
                    Expect.false (toString err) True

        Ok wasv ->
            case sb of
                Err _ ->
                    Expect.false "Expected an error but didn't get one." True

                Ok sbv ->
                    Expect.equal sbv wasv


doTest : ( String, a, a ) -> Test
doTest ( name, was, sb ) =
    test name
        (\_ ->
            expectResult (Ok sb) (Ok was)
        )


lo1 =
    128 + 1


hi1 =
    128 + 3


lo2 =
    128 + 64 + 1


hi2 =
    128 + 64 + 3


lo3 =
    128 + 64 + 4 + 1


hi3 =
    128 + 64 + 4 + 3


lo4 =
    128 + 64 + 8 + 4 + 1


hi4 =
    128 + 64 + 8 + 4 + 3


word =
    makeword hi1 lo1


intData : List ( String, Int, Int )
intData =
    [ ( "1+1", 1 + 1, 2 )
    , ( "lobyte", lobyte word, lo1 )
    , ( "hibyte", hibyte word, hi1 )
    , ( "swapbytes", swapbytes word, makeword lo1 hi1 )
    ]


word2 =
    makeword hi2 lo2


word3 =
    makeword hi3 lo3


word4 =
    makeword hi4 lo4


array =
    fromList [ word, word2, word3, word4 ]


array_rotatePairsRight =
    fromList
        [ makeword lo2 hi1
        , makeword lo1 hi2
        , makeword lo4 hi3
        , makeword lo3 hi4
        ]


arrayData : List ( String, Array Int, Array Int )
arrayData =
    [ ( "rotatePairsRight", arrayRotatePairsRight array, array_rotatePairsRight )
    ]
