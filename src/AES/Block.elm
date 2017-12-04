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
import Array exposing (Array, empty, fromList, get, length, repeat, set)
import BitwiseInfix exposing (..)


{-| Expand a raw key array.
-}
expandKey : Array Int -> Keys
expandKey rawkey =
    { numRounds = 0
    , forwardKey = empty
    , reverseKey = empty
    }


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
