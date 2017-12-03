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

import Array exposing (Array, empty, fromList, get, length, repeat, set)
import BitwiseInfix exposing (..)


lobyte : Int -> Int
lobyte x =
    x ~& 255


hibyte : Int -> Int
hibyte x =
    (x ~>> 8) ~& 255


swapbytes : Int -> Int
swapbytes x =
    ((x ~& 255) ~<< 8) ~| ((x ~>> 8) ~& 255)


arrayRotatePairsRight : Array Int -> Array Int
arrayRotatePairsRight a =
    a
