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


module AES
    exposing
        ( decrypt
        , encrypt
        )

{-| A pure Elm implementation of Rijndael, the Advanced Encryption Standard (AES).

Based on [cl-cryto](https://github.com/billstclair/cl-crypto)'s [aes16.lisp](https://github.com/billstclair/cl-crypto/blob/master/source/aes16.lisp).


# Functions

@docs encrypt, decrypt

-}

import Array exposing (Array, empty, fromList, get, length, repeat, set)


type alias Keys =
    { numRounds : Int
    , forwardKey : Array Int
    , reverseKey : Array Int
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
