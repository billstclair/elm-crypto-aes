----------------------------------------------------------------------
--
-- AES.elm
-- Elm implementation of AES, the Advanced Encryption Standard.
-- Copyright (c) 2017 Bill St. Clair <billstclair@gmail.com>
-- Some rights reserved.
-- Distributed under the MIT License
-- See LICENSE.txt
--
----------------------------------------------------------------------


module AES.Utility exposing (..)

import Array exposing (Array, empty, fromList, length, repeat, set)
import BitwiseInfix exposing (..)


{-| Out of bounds array references return 0.

Don't be naughty.

Maybe they should Debug.crash, but that's not nice.

-}
get : Int -> Array Int -> Int
get idx array =
    Maybe.withDefault 0 <|
        Array.get idx array


lobyte : Int -> Int
lobyte x =
    x ~& 255


hibyte : Int -> Int
hibyte x =
    (x ~>> 8) ~& 255


makeword : Int -> Int -> Int
makeword hi lo =
    (hi ~<< 8) + lo


swapbytes : Int -> Int
swapbytes x =
    ((x ~& 255) ~<< 8) ~| ((x ~>> 8) ~& 255)


byte0 =
    lobyte


byte1 =
    hibyte


byte2 : Int -> Int
byte2 x =
    (x ~>> 16) ~& 255


byte3 : Int -> Int
byte3 x =
    (x ~>> 24) ~& 255


word0 : Int -> Int
word0 x =
    x ~& 65535


word1 : Int -> Int
word1 x =
    (x ~>> 16) ~& 65535


{-| rot-uint-32-L, but only handles n = 8
-}
rotWord32L : Int -> Int
rotWord32L word =
    ((word ~& 0x00FFFFFF) ~<< 8)
        + ((word ~>> 24) ~& 0xFF)


{-| Returns new array with right byte rotation across two adjacent uint-16 values.

array-rotate-uint-16-pairs-right

-}
arrayRotatePairsRight : Array Int -> Array Int
arrayRotatePairsRight gin =
    let
        gnumWords =
            length gin

        loop : Int -> Array Int -> Array Int
        loop =
            \gi res ->
                if gi >= gnumWords then
                    res
                else
                    let
                        gw1 =
                            get gi gin

                        gw0 =
                            get (gi + 1) gin

                        gout =
                            set gi
                                (makeword (lobyte gw0) (hibyte gw1))
                            <|
                                set (gi + 1)
                                    (makeword (lobyte gw1) (hibyte gw0))
                                    res
                    in
                    loop (gi + 2) gout
    in
    loop 0 <| repeat gnumWords 0



---
--- AES Support Routines
---


hexStr2Int : String -> Result String Int
hexStr2Int str =
    String.toInt ("0x" ++ str)


hexChars2Int : Char -> Char -> Result String Int
hexChars2Int x y =
    hexStr2Int <| String.fromList [ x, y ]


{-| hex-str->bin-array
-}
hexStr2Array : String -> Result String (Array Int)
hexStr2Array string =
    let
        loop : List Char -> List Int -> Result String (Array Int)
        loop =
            \chars res ->
                case chars of
                    x :: y :: tail ->
                        case hexChars2Int x y of
                            Err msg ->
                                Err msg

                            Ok int ->
                                loop tail <| int :: res

                    [ _ ] ->
                        Err "Odd length string."

                    _ ->
                        Ok (fromList <| List.reverse res)
    in
    loop (String.toList string) []


cb : String -> Array Int
cb string =
    Result.withDefault empty <| hexStr2Array string


{-| make-bytes-from-uint-16
-}
makeBytesFromWord : Int -> Int -> Array Int -> Array Int
makeBytesFromWord word offset array =
    set offset (hibyte word) <|
        set (offset + 1) (lobyte word) array


{-| make-uint-16-from-byte-array
-}
makeWordFromByteArray : Int -> Array Int -> Int
makeWordFromByteArray offset array =
    makeword (get offset array) (get (offset + 1) array)


{-| fill-byte-array-from-uint-16s
-}
fillByteArrayFromWords : List Int -> Array Int
fillByteArrayFromWords words =
    let
        f : Int -> ( Int, Array Int ) -> ( Int, Array Int )
        f =
            \word ( idx, res ) ->
                ( idx + 2, makeBytesFromWord word idx res )

        out =
            repeat (2 * List.length words) 0

        ( _, res ) =
            List.foldl f ( 0, out ) words
    in
    res


{-| make-uint-32
-}
makeWord32 : Int -> Int -> Int -> Int -> Int
makeWord32 b3 b2 b1 b0 =
    (b3 ~<< 24) + (b2 ~<< 16) + (b1 ~<< 8) + b0


{-| make-uint-32-from-byte-array
-}
makeWord32FromByteArray : Int -> Array Int -> Int
makeWord32FromByteArray offset array =
    makeWord32
        (get offset array)
        (get (1 + offset) array)
        (get (2 + offset) array)
        (get (3 + offset) array)



---
--- AES Key Expansion Support
---


{-| uint-32-array->uint-16-array
-}
word32ArrayToWordArray : Array Int -> Array Int
word32ArrayToWordArray a =
    let
        f : Int -> ( Int, Array Int ) -> ( Int, Array Int )
        f =
            \ai ( j, res ) ->
                ( j + 2
                , set j (word1 ai) res
                    |> set (j + 1) (word0 ai)
                )

        out =
            repeat (2 * length a) 0

        ( _, res ) =
            Array.foldl f ( 0, out ) a
    in
    res
