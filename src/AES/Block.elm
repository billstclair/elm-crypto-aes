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

import AES.Tables exposing (..)
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
    case hexStr2Array hex of
        Err msg ->
            Err msg

        Ok rawkey ->
            expandKey rawkey


zeroKeys : Keys
zeroKeys =
    { numRounds = 0
    , forwardKey = empty
    , reverseKey = empty
    }


expandKeyStringNow : String -> Keys
expandKeyStringNow string =
    Result.withDefault zeroKeys <|
        expandKeyString string



---
--- AES encrypt/decrypt routines
---


type alias Pair =
    ( Int, Int )


type alias Quartet =
    ( Int, Int, Int, Int )


type alias FourPairs =
    ( Pair, Pair, Pair, Pair )


{-| aes-round-step
-}
roundStep : ( Array Int, Array Int, Array Int, Array Int ) -> Pair -> Quartet -> Pair
roundStep ( t0, t1, t2, t3 ) ( rkh, rkl ) ( b0, b1, b2, b3 ) =
    let
        g0 =
            2 * b0

        g1 =
            2 * b1

        g2 =
            2 * b2

        g3 =
            2 * b3
    in
    ( rkh
        ~^ get g0 t0
        ~^ get g1 t1
        ~^ get g2 t2
        ~^ get g3 t3
    , rkl
        ~^ get (1 + g0) t0
        ~^ get (1 + g1) t1
        ~^ get (1 + g2) t2
        ~^ get (1 + g3) t3
    )


{-| aes-last-round-step
-}
lastRoundStep : Array Int -> Pair -> Quartet -> Pair
lastRoundStep sb ( rkh, rkl ) ( b0, b1, b2, b3 ) =
    ( rkh
        ~^ (get b0 sb ~<< 8)
        ~^ get b1 sb
    , rkl
        ~^ (get b2 sb ~<< 8)
        ~^ get b3 sb
    )


fts_ : ( Array Int, Array Int, Array Int, Array Int )
fts_ =
    ( ft0_, ft1_, ft2_, ft3_ )


{-| aes-f-round
-}
fRound : Array Int -> Int -> FourPairs -> FourPairs
fRound keys rkix ws =
    let
        ( ( w0h, w0l ), ( w1h, w1l ), ( w2h, w2l ), ( w3h, w3l ) ) =
            ws

        x0 =
            roundStep fts_
                ( get rkix keys, get (1 + rkix) keys )
                ( hibyte w0h, lobyte w1h, hibyte w2l, lobyte w3l )

        x1 =
            roundStep fts_
                ( get (2 + rkix) keys, get (3 + rkix) keys )
                ( hibyte w1h, lobyte w2h, hibyte w3l, lobyte w0l )

        x2 =
            roundStep fts_
                ( get (4 + rkix) keys, get (5 + rkix) keys )
                ( hibyte w2h, lobyte w3h, hibyte w0l, lobyte w1l )

        x3 =
            roundStep fts_
                ( get (6 + rkix) keys, get (7 + rkix) keys )
                ( hibyte w3h, lobyte w0h, hibyte w1l, lobyte w2l )
    in
    ( x0, x1, x2, x3 )


{-| aes-last-f-round
-}
lastFRound : Array Int -> Int -> FourPairs -> FourPairs
lastFRound keys rkix ws =
    let
        ( ( w0h, w0l ), ( w1h, w1l ), ( w2h, w2l ), ( w3h, w3l ) ) =
            ws

        x0 =
            lastRoundStep fsb_
                ( get rkix keys, get (1 + rkix) keys )
                ( hibyte w0h, lobyte w1h, hibyte w2l, lobyte w3l )

        x1 =
            lastRoundStep fsb_
                ( get (2 + rkix) keys, get (3 + rkix) keys )
                ( hibyte w1h, lobyte w2h, hibyte w3l, lobyte w0l )

        x2 =
            lastRoundStep fsb_
                ( get (4 + rkix) keys, get (5 + rkix) keys )
                ( hibyte w2h, lobyte w3h, hibyte w0l, lobyte w1l )

        x3 =
            lastRoundStep fsb_
                ( get (6 + rkix) keys, get (7 + rkix) keys )
                ( hibyte w3h
                , lobyte w0h
                , hibyte w1l
                , lobyte w2l
                )
    in
    ( x0, x1, x2, x3 )



--- Reverse


rts_ : ( Array Int, Array Int, Array Int, Array Int )
rts_ =
    ( rt0_, rt1_, rt2_, rt3_ )


{-| aes-r-round
-}
rRound : Array Int -> Int -> FourPairs -> FourPairs
rRound keys rkix ws =
    let
        ( ( w0h, w0l ), ( w1h, w1l ), ( w2h, w2l ), ( w3h, w3l ) ) =
            ws

        x0 =
            roundStep rts_
                ( get rkix keys, get (1 + rkix) keys )
                ( hibyte w0h, lobyte w3h, hibyte w2l, lobyte w1l )

        x1 =
            roundStep rts_
                ( get (2 + rkix) keys, get (3 + rkix) keys )
                ( hibyte w1h, lobyte w0h, hibyte w3l, lobyte w2l )

        x2 =
            roundStep rts_
                ( get (4 + rkix) keys, get (5 + rkix) keys )
                ( hibyte w2h, lobyte w1h, hibyte w0l, lobyte w3l )

        x3 =
            roundStep rts_
                ( get (6 + rkix) keys, get (7 + rkix) keys )
                ( hibyte w3h, lobyte w2h, hibyte w1l, lobyte w0l )
    in
    ( x0, x1, x2, x3 )


{-| aes-last-r-round
-}
lastRRound : Array Int -> Int -> FourPairs -> FourPairs
lastRRound keys rkix ws =
    let
        ( ( w0h, w0l ), ( w1h, w1l ), ( w2h, w2l ), ( w3h, w3l ) ) =
            ws

        x0 =
            lastRoundStep rsb_
                ( get rkix keys, get (1 + rkix) keys )
                ( hibyte w0h, lobyte w3h, hibyte w2l, lobyte w1l )

        x1 =
            lastRoundStep rsb_
                ( get (2 + rkix) keys, get (3 + rkix) keys )
                ( hibyte w1h, lobyte w0h, hibyte w3l, lobyte w2l )

        x2 =
            lastRoundStep rsb_
                ( get (4 + rkix) keys, get (5 + rkix) keys )
                ( hibyte w2h, lobyte w1h, hibyte w0l, lobyte w3l )

        x3 =
            lastRoundStep rsb_
                ( get (6 + rkix) keys, get (7 + rkix) keys )
                ( hibyte w3h, lobyte w2h, hibyte w1l, lobyte w0l )
    in
    ( x0, x1, x2, x3 )


{-| with-init-aes-vars
-}
loadKeys : Array Int -> Array Int -> FourPairs
loadKeys ina keys =
    let
        getOne : Int -> Int
        getOne =
            \idx ->
                get idx keys
                    ~^ makeWordFromByteArray (2 * idx) ina

        f : Int -> List Pair -> List Pair
        f =
            \idx res ->
                ( getOne idx, getOne <| idx + 1 ) :: res

        list =
            List.foldr f [] [ 0, 2, 4, 6 ]
    in
    case list of
        [ a, b, c, d ] ->
            ( a, b, c, d )

        _ ->
            -- Can't happen
            ( ( 0, 0 ), ( 0, 0 ), ( 0, 0 ), ( 0, 0 ) )


fillByteArrayFromFourPairs : FourPairs -> Array Int
fillByteArrayFromFourPairs ws =
    let
        ( ( w0h, w0l ), ( w1h, w1l ), ( w2h, w2l ), ( w3h, w3l ) ) =
            ws
    in
    fillByteArrayFromWords [ w0h, w0l, w1h, w1l, w2h, w2l, w3h, w3l ]


{-| Encrypt the 16-element Array with the Key
-}
encrypt : Keys -> Array Int -> Array Int
encrypt keys ina =
    let
        numRounds =
            keys.numRounds

        keya =
            keys.forwardKey

        loop : Int -> Int -> FourPairs -> FourPairs
        loop =
            \i rkix ws ->
                if i >= numRounds then
                    ws
                else
                    loop (1 + i) (8 + rkix) <|
                        fRound keya rkix ws
    in
    loop 1 8 (loadKeys ina keya)
        |> fRound keya (8 * (numRounds - 1))
        |> fillByteArrayFromFourPairs


{-| Decrypt the 16-element Array with the Key
-}
decrypt : Keys -> Array Int -> Array Int
decrypt keys ina =
    ina
