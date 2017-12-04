----------------------------------------------------------------------
--
-- Block.elm
-- The 16-element array basis of the AES algorithm.
-- Copyright (c) 2017 Bill St. Clair <billstclair@gmail.com>
-- Some rights reserved.
-- Distributed under the MIT License
-- See LICENSE.txt
--
----------------------------------------------------------------------


module AES.Block exposing (..)

import AES.Types exposing (Keys)
import AES.Utility exposing (..)
import Array exposing (Array, empty, fromList, length, repeat, set)
import BitwiseInfix exposing (..)
import Debug
import List.Extra as LE


dlog =
    Debug.log


{-| prepare-reverse-key
-}
prepareReverseKey : Array Int -> Int -> Array Int
prepareReverseKey fkey numRounds =
    let
        numkeys =
            length fkey

        rkey =
            repeat numkeys 0

        loop1 : Int -> Array Int -> Array Int
        loop1 =
            \i res ->
                if i >= 4 then
                    res
                else
                    loop1 (1 + i)
                        (set i (get (i + numkeys - 4) fkey) res
                            |> set (i + numkeys - 4) (get i fkey)
                        )

        loop2 : Int -> Array Int -> Array Int
        loop2 =
            \r res ->
                if r >= numRounds - 1 then
                    res
                else
                    let
                        fix =
                            numkeys - (4 * (2 + r))

                        rix =
                            4 * (1 + r)
                    in
                    loop2 (r + 1) <|
                        inner fix rix 0 res

        inner : Int -> Int -> Int -> Array Int -> Array Int
        inner =
            \fix rix i res ->
                if i >= 4 then
                    res
                else
                    let
                        tmp =
                            get (fix + i) fkey

                        out =
                            res
                                |> set (rix + i)
                                    (get (byte3 tmp) kt0_
                                        ~^ get (byte2 tmp) kt1_
                                        ~^ get (byte1 tmp) kt2_
                                        ~^ get (byte0 tmp) kt3_
                                    )
                    in
                    inner fix rix (i + 1) out
    in
    loop1 0 rkey
        |> loop2 0


{-| Expand a raw key array.
-}
expandKey : Array Int -> Result String Keys
expandKey rawkey =
    let
        len =
            length rawkey
    in
    case
        LE.find (\( n, _, _ ) -> len == n)
            [ ( 16, 4, 10 )
            , ( 24, 6, 12 )
            , ( 32, 8, 14 )
            ]
    of
        Nothing ->
            Err "Invalid key size. Must be 16, 24, or 32 bytes."

        Just ( _, numWords, numRounds ) ->
            Ok <|
                expandKeyInternal rawkey numWords numRounds


expandKeyInternal : Array Int -> Int -> Int -> Keys
expandKeyInternal rawkey numWords numRounds =
    let
        loop1 : Int -> Array Int -> Array Int
        loop1 =
            \i res ->
                if i >= numWords then
                    res
                else
                    loop1 (1 + i) <|
                        set i (makeWord32FromByteArray (i * 4) rawkey) res

        loop2 : Int -> Array Int -> Array Int
        loop2 =
            \i res ->
                if i >= size then
                    res
                else
                    res
                        |> (set i <|
                                get (i - numWords) res
                                    ~^ (if 0 == i % numWords then
                                            get ((i // numWords) - 1) rcon_
                                                ~^ subWord32 (rotWord32L <| get (i - 1) res)
                                        else if numWords > 6 && 4 == i % numWords then
                                            subWord32 <| get (i - 1) res
                                        else
                                            get (i - 1) res
                                       )
                           )
                        |> loop2 (i + 1)

        size =
            blockWords_ * (numRounds + 1)

        fkey =
            loop1 0 (repeat size 0)
                |> loop2 numWords
    in
    { numRounds = numRounds
    , forwardKey = word32ArrayToWordArray fkey
    , reverseKey = word32ArrayToWordArray <| prepareReverseKey fkey numRounds
    }


expandKeyString : String -> Result String Keys
expandKeyString hex =
    expandKey <| hexStr2Array hex


zeroKeys : Keys
zeroKeys =
    { numRounds = 0
    , forwardKey = empty
    , reverseKey = empty
    }


expandKeyString4Sure : String -> Keys
expandKeyString4Sure string =
    Result.withDefault zeroKeys <|
        expandKeyString string


{-| Encrypt the 16-element Array with the Key
-}
encrypt : Keys -> Array Int -> Array Int
encrypt keys ina =
    ina


{-| Decrypt the 16-element Array with the Key
-}
decrypt : Keys -> Array Int -> Array Int
decrypt keys ina =
    ina
