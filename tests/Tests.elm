module Tests exposing (all)

import AES exposing (..)
import AES.Types exposing (..)
import AES.Utility exposing (..)
import Expect exposing (Expectation)
import List
import Maybe exposing (withDefault)
import Test exposing (..)


log =
    Debug.log



--change to True to log JSON input & output results


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
            [ List.map intTest intData
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


intTest : ( String, Int, Int ) -> Test
intTest ( name, was, sb ) =
    test ("intTest \"" ++ name ++ "\"")
        (\_ ->
            expectResult (Ok sb) (Ok was)
        )


intData : List ( String, Int, Int )
intData =
    let
        lobyt =
            129

        hibyt =
            131

        word =
            (hibyt * 256) + lobyt

        swappedByte =
            hibyt + (lobyt * 256)
    in
    [ ( "1+1", 1 + 1, 2 )
    , ( "lobyte", lobyte word, lobyt )
    , ( "hibyte", hibyte word, hibyt )
    , ( "swapbytes", swapbytes word, swappedByte )
    ]
