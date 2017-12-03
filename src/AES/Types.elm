----------------------------------------------------------------------
--
-- Types.elm
-- Types for billstclair/elm-aes
-- Copyright (c) 2017 Bill St. Clair <billstclair@gmail.com>
-- Some rights reserved.
-- Distributed under the MIT License
-- See LICENSE.txt
--
----------------------------------------------------------------------


module AES.Types exposing (Keys)

{-| Types for the AES module.

@docs Keys

-}

import Array exposing (Array, empty, fromList, get, length, repeat, set)


{-| Encryption & decryption keys for AES algorithm.
-}
type alias Keys =
    { numRounds : Int
    , forwardKey : Array Int
    , reverseKey : Array Int
    }
