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
        , expandKeyString
        )

{-| A pure Elm implementation of Rijndael, the Advanced Encryption Standard (AES).

Based on [cl-cryto](https://github.com/billstclair/cl-crypto)'s [aes16.lisp](https://github.com/billstclair/cl-crypto/blob/master/source/aes16.lisp).


# Functions

@docs expandKey, expandKeyString, encrypt, decrypt

-}

import AES.Block
import AES.Types exposing (Keys)
import Array exposing (Array)


{-| Expand a raw key array.
-}
expandKey : Array Int -> Result String Keys
expandKey =
    AES.Block.expandKey


{-| Expand a raw key represented as a string of Hex characters.
-}
expandKeyString : String -> Result String Keys
expandKeyString =
    AES.Block.expandKeyString


{-| Encrypt the 16-element Array with the Keys
-}
encrypt : Keys -> Array Int -> Array Int
encrypt =
    AES.Block.encrypt


{-| Decrypt the 16-element Array with the Keys
-}
decrypt : Keys -> Array Int -> Array Int
decrypt =
    AES.Block.decrypt
