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


module Crypto.AES exposing
    ( Keys
    , expandKey, expandKeyString, encrypt, decrypt
    )

{-| A pure Elm implementation of Rijndael, the Advanced Encryption Standard (AES).

Based on [cl-cryto](https://github.com/billstclair/cl-crypto)'s [aes16.lisp](https://github.com/billstclair/cl-crypto/blob/master/source/aes16.lisp).


# Types

@docs Keys


# Functions

@docs expandKey, expandKeyString, encrypt, decrypt

-}

import Array exposing (Array)
import Crypto.AES.Block


{-| Expanded AES encryption and decryption keys.

Created by `expandKey` and `expandKeyString`. Passed to `encryt` and `decrypt`.

-}
type Keys
    = Keys Crypto.AES.Block.Keys


{-| Expand a raw key array.

The array's length must be 16, 24, or 32 (128, 192, or 256 bits), and its elements must all be between 0 and 255 inclusive, or you'll get an error.

-}
expandKey : Array Int -> Result String Keys
expandKey rawkey =
    if Crypto.AES.Block.validateKeyElements rawkey then
        case Crypto.AES.Block.expandKey rawkey of
            Ok keys ->
                Ok <| Keys keys

            Err msg ->
                Err msg

    else
        Err "Key elements must be between 0 and 255 inclusive."


{-| Expand a raw key represented as a string of Hex characters.

All the characters must be in the ranges 0-9, A-F (a-f), and there must be 32, 48, or 64 of them, or your'll get an error.

-}
expandKeyString : String -> Result String Keys
expandKeyString string =
    case Crypto.AES.Block.expandKeyString string of
        Ok keys ->
            Ok <| Keys keys

        Err msg ->
            Err msg


{-| Encrypt the 16-element Array with the Keys.

If the array has other than 16 elements, or any of them are not in the range 0-255, you'll get unexpected results.

-}
encrypt : Keys -> Array Int -> Array Int
encrypt (Keys keys) a =
    Crypto.AES.Block.encrypt keys a


{-| Decrypt the 16-element Array with the Keys

If the array has other than 16 elements, or any of them are not in the range 0-255, you'll get unexpected results.

-}
decrypt : Keys -> Array Int -> Array Int
decrypt (Keys keys) a =
    Crypto.AES.Block.decrypt keys a
