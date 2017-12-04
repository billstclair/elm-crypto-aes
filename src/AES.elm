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
        , expandKey
        )

{-| A pure Elm implementation of Rijndael, the Advanced Encryption Standard (AES).

Based on [cl-cryto](https://github.com/billstclair/cl-crypto)'s [aes16.lisp](https://github.com/billstclair/cl-crypto/blob/master/source/aes16.lisp).


# Functions

@docs expandKey, encrypt, decrypt

-}

import AES.Block exposing (decrypt, encrypt, expandKey)
import AES.Types exposing (Keys)
import AES.Utility exposing (..)
import Array exposing (Array, empty, fromList, get, length, repeat, set)
import BitwiseInfix exposing (..)


{-| Expand a raw key array.
-}
expandKey : Array Int -> Keys
expandKey =
    AES.Block.expandKey


{-| Encrypt the 16-element Array with the Key
-}
encrypt : Keys -> Array Int -> Array Int
encrypt =
    AES.Block.encrypt


{-| Decrypt the 16-element Array with the Key
-}
decrypt : Keys -> Array Int -> Array Int
decrypt =
    AES.Block.decrypt
