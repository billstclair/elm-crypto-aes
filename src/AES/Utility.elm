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


get : Int -> Array Int -> Int
get idx array =
    Maybe.withDefault 0 (Array.get idx array)


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


{-| Returns new array with right byte rotation across two adjacent uint-16 values.

lisp: array-rotate-uint-16-pairs-right

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


{-| Generate helper tables for reverse key schedule using equivalent inverse algorithm.

lisp: gen-k-table

-}
genKTable : Array Int -> Array Int
genKTable gin =
    let
        gsize =
            length gin // 2

        loop : Int -> Array Int -> Array Int
        loop =
            \i res ->
                if i >= gsize then
                    res
                else
                    let
                        idx =
                            2 * get i fsb_
                    in
                    loop (i + 1)
                        (res
                            |> set i
                                ((get idx gin ~<< 16)
                                    ~| get (1 + idx) gin
                                )
                        )
    in
    loop 0 <| repeat gsize 0



---
--- Constants
---


blockWords_ : Int
blockWords_ =
    4


rcon_ : Array Int
rcon_ =
    fromList <|
        List.concat
            [ [ 0x01000000, 0x02000000, 0x04000000, 0x08000000, 0x10000000 ]
            , [ 0x20000000, 0x40000000, 0x80000000, 0x1B000000, 0x36000000 ]
            ]


fsb_ : Array Int
fsb_ =
    fromList <|
        List.concat
            [ [ 0x63, 0x7C, 0x77, 0x7B, 0xF2, 0x6B, 0x6F, 0xC5 ]
            , [ 0x30, 0x01, 0x67, 0x2B, 0xFE, 0xD7, 0xAB, 0x76 ]
            , [ 0xCA, 0x82, 0xC9, 0x7D, 0xFA, 0x59, 0x47, 0xF0 ]
            , [ 0xAD, 0xD4, 0xA2, 0xAF, 0x9C, 0xA4, 0x72, 0xC0 ]
            , [ 0xB7, 0xFD, 0x93, 0x26, 0x36, 0x3F, 0xF7, 0xCC ]
            , [ 0x34, 0xA5, 0xE5, 0xF1, 0x71, 0xD8, 0x31, 0x15 ]
            , [ 0x04, 0xC7, 0x23, 0xC3, 0x18, 0x96, 0x05, 0x9A ]
            , [ 0x07, 0x12, 0x80, 0xE2, 0xEB, 0x27, 0xB2, 0x75 ]
            , [ 0x09, 0x83, 0x2C, 0x1A, 0x1B, 0x6E, 0x5A, 0xA0 ]
            , [ 0x52, 0x3B, 0xD6, 0xB3, 0x29, 0xE3, 0x2F, 0x84 ]
            , [ 0x53, 0xD1, 0x00, 0xED, 0x20, 0xFC, 0xB1, 0x5B ]
            , [ 0x6A, 0xCB, 0xBE, 0x39, 0x4A, 0x4C, 0x58, 0xCF ]
            , [ 0xD0, 0xEF, 0xAA, 0xFB, 0x43, 0x4D, 0x33, 0x85 ]
            , [ 0x45, 0xF9, 0x02, 0x7F, 0x50, 0x3C, 0x9F, 0xA8 ]
            , [ 0x51, 0xA3, 0x40, 0x8F, 0x92, 0x9D, 0x38, 0xF5 ]
            , [ 0xBC, 0xB6, 0xDA, 0x21, 0x10, 0xFF, 0xF3, 0xD2 ]
            , [ 0xCD, 0x0C, 0x13, 0xEC, 0x5F, 0x97, 0x44, 0x17 ]
            , [ 0xC4, 0xA7, 0x7E, 0x3D, 0x64, 0x5D, 0x19, 0x73 ]
            , [ 0x60, 0x81, 0x4F, 0xDC, 0x22, 0x2A, 0x90, 0x88 ]
            , [ 0x46, 0xEE, 0xB8, 0x14, 0xDE, 0x5E, 0x0B, 0xDB ]
            , [ 0xE0, 0x32, 0x3A, 0x0A, 0x49, 0x06, 0x24, 0x5C ]
            , [ 0xC2, 0xD3, 0xAC, 0x62, 0x91, 0x95, 0xE4, 0x79 ]
            , [ 0xE7, 0xC8, 0x37, 0x6D, 0x8D, 0xD5, 0x4E, 0xA9 ]
            , [ 0x6C, 0x56, 0xF4, 0xEA, 0x65, 0x7A, 0xAE, 0x08 ]
            , [ 0xBA, 0x78, 0x25, 0x2E, 0x1C, 0xA6, 0xB4, 0xC6 ]
            , [ 0xE8, 0xDD, 0x74, 0x1F, 0x4B, 0xBD, 0x8B, 0x8A ]
            , [ 0x70, 0x3E, 0xB5, 0x66, 0x48, 0x03, 0xF6, 0x0E ]
            , [ 0x61, 0x35, 0x57, 0xB9, 0x86, 0xC1, 0x1D, 0x9E ]
            , [ 0xE1, 0xF8, 0x98, 0x11, 0x69, 0xD9, 0x8E, 0x94 ]
            , [ 0x9B, 0x1E, 0x87, 0xE9, 0xCE, 0x55, 0x28, 0xDF ]
            , [ 0x8C, 0xA1, 0x89, 0x0D, 0xBF, 0xE6, 0x42, 0x68 ]
            , [ 0x41, 0x99, 0x2D, 0x0F, 0xB0, 0x54, 0xBB, 0x16 ]
            ]


rsb_ : Array Int
rsb_ =
    fromList <|
        List.concat
            [ [ 0x52, 0x09, 0x6A, 0xD5, 0x30, 0x36, 0xA5, 0x38 ]
            , [ 0xBF, 0x40, 0xA3, 0x9E, 0x81, 0xF3, 0xD7, 0xFB ]
            , [ 0x7C, 0xE3, 0x39, 0x82, 0x9B, 0x2F, 0xFF, 0x87 ]
            , [ 0x34, 0x8E, 0x43, 0x44, 0xC4, 0xDE, 0xE9, 0xCB ]
            , [ 0x54, 0x7B, 0x94, 0x32, 0xA6, 0xC2, 0x23, 0x3D ]
            , [ 0xEE, 0x4C, 0x95, 0x0B, 0x42, 0xFA, 0xC3, 0x4E ]
            , [ 0x08, 0x2E, 0xA1, 0x66, 0x28, 0xD9, 0x24, 0xB2 ]
            , [ 0x76, 0x5B, 0xA2, 0x49, 0x6D, 0x8B, 0xD1, 0x25 ]
            , [ 0x72, 0xF8, 0xF6, 0x64, 0x86, 0x68, 0x98, 0x16 ]
            , [ 0xD4, 0xA4, 0x5C, 0xCC, 0x5D, 0x65, 0xB6, 0x92 ]
            , [ 0x6C, 0x70, 0x48, 0x50, 0xFD, 0xED, 0xB9, 0xDA ]
            , [ 0x5E, 0x15, 0x46, 0x57, 0xA7, 0x8D, 0x9D, 0x84 ]
            , [ 0x90, 0xD8, 0xAB, 0x00, 0x8C, 0xBC, 0xD3, 0x0A ]
            , [ 0xF7, 0xE4, 0x58, 0x05, 0xB8, 0xB3, 0x45, 0x06 ]
            , [ 0xD0, 0x2C, 0x1E, 0x8F, 0xCA, 0x3F, 0x0F, 0x02 ]
            , [ 0xC1, 0xAF, 0xBD, 0x03, 0x01, 0x13, 0x8A, 0x6B ]
            , [ 0x3A, 0x91, 0x11, 0x41, 0x4F, 0x67, 0xDC, 0xEA ]
            , [ 0x97, 0xF2, 0xCF, 0xCE, 0xF0, 0xB4, 0xE6, 0x73 ]
            , [ 0x96, 0xAC, 0x74, 0x22, 0xE7, 0xAD, 0x35, 0x85 ]
            , [ 0xE2, 0xF9, 0x37, 0xE8, 0x1C, 0x75, 0xDF, 0x6E ]
            , [ 0x47, 0xF1, 0x1A, 0x71, 0x1D, 0x29, 0xC5, 0x89 ]
            , [ 0x6F, 0xB7, 0x62, 0x0E, 0xAA, 0x18, 0xBE, 0x1B ]
            , [ 0xFC, 0x56, 0x3E, 0x4B, 0xC6, 0xD2, 0x79, 0x20 ]
            , [ 0x9A, 0xDB, 0xC0, 0xFE, 0x78, 0xCD, 0x5A, 0xF4 ]
            , [ 0x1F, 0xDD, 0xA8, 0x33, 0x88, 0x07, 0xC7, 0x31 ]
            , [ 0xB1, 0x12, 0x10, 0x59, 0x27, 0x80, 0xEC, 0x5F ]
            , [ 0x60, 0x51, 0x7F, 0xA9, 0x19, 0xB5, 0x4A, 0x0D ]
            , [ 0x2D, 0xE5, 0x7A, 0x9F, 0x93, 0xC9, 0x9C, 0xEF ]
            , [ 0xA0, 0xE0, 0x3B, 0x4D, 0xAE, 0x2A, 0xF5, 0xB0 ]
            , [ 0xC8, 0xEB, 0xBB, 0x3C, 0x83, 0x53, 0x99, 0x61 ]
            , [ 0x17, 0x2B, 0x04, 0x7E, 0xBA, 0x77, 0xD6, 0x26 ]
            , [ 0xE1, 0x69, 0x14, 0x63, 0x55, 0x21, 0x0C, 0x7D ]
            ]


ft0_ : Array Int
ft0_ =
    fromList <|
        List.concat
            [ [ 0xC663, 0x63A5, 0xF87C, 0x7C84, 0xEE77, 0x7799, 0xF67B, 0x7B8D ]
            , [ 0xFFF2, 0xF20D, 0xD66B, 0x6BBD, 0xDE6F, 0x6FB1, 0x91C5, 0xC554 ]
            , [ 0x6030, 0x3050, 0x0201, 0x0103, 0xCE67, 0x67A9, 0x562B, 0x2B7D ]
            , [ 0xE7FE, 0xFE19, 0xB5D7, 0xD762, 0x4DAB, 0xABE6, 0xEC76, 0x769A ]
            , [ 0x8FCA, 0xCA45, 0x1F82, 0x829D, 0x89C9, 0xC940, 0xFA7D, 0x7D87 ]
            , [ 0xEFFA, 0xFA15, 0xB259, 0x59EB, 0x8E47, 0x47C9, 0xFBF0, 0xF00B ]
            , [ 0x41AD, 0xADEC, 0xB3D4, 0xD467, 0x5FA2, 0xA2FD, 0x45AF, 0xAFEA ]
            , [ 0x239C, 0x9CBF, 0x53A4, 0xA4F7, 0xE472, 0x7296, 0x9BC0, 0xC05B ]
            , [ 0x75B7, 0xB7C2, 0xE1FD, 0xFD1C, 0x3D93, 0x93AE, 0x4C26, 0x266A ]
            , [ 0x6C36, 0x365A, 0x7E3F, 0x3F41, 0xF5F7, 0xF702, 0x83CC, 0xCC4F ]
            , [ 0x6834, 0x345C, 0x51A5, 0xA5F4, 0xD1E5, 0xE534, 0xF9F1, 0xF108 ]
            , [ 0xE271, 0x7193, 0xABD8, 0xD873, 0x6231, 0x3153, 0x2A15, 0x153F ]
            , [ 0x0804, 0x040C, 0x95C7, 0xC752, 0x4623, 0x2365, 0x9DC3, 0xC35E ]
            , [ 0x3018, 0x1828, 0x3796, 0x96A1, 0x0A05, 0x050F, 0x2F9A, 0x9AB5 ]
            , [ 0x0E07, 0x0709, 0x2412, 0x1236, 0x1B80, 0x809B, 0xDFE2, 0xE23D ]
            , [ 0xCDEB, 0xEB26, 0x4E27, 0x2769, 0x7FB2, 0xB2CD, 0xEA75, 0x759F ]
            , [ 0x1209, 0x091B, 0x1D83, 0x839E, 0x582C, 0x2C74, 0x341A, 0x1A2E ]
            , [ 0x361B, 0x1B2D, 0xDC6E, 0x6EB2, 0xB45A, 0x5AEE, 0x5BA0, 0xA0FB ]
            , [ 0xA452, 0x52F6, 0x763B, 0x3B4D, 0xB7D6, 0xD661, 0x7DB3, 0xB3CE ]
            , [ 0x5229, 0x297B, 0xDDE3, 0xE33E, 0x5E2F, 0x2F71, 0x1384, 0x8497 ]
            , [ 0xA653, 0x53F5, 0xB9D1, 0xD168, 0x00, 0x00, 0xC1ED, 0xED2C ]
            , [ 0x4020, 0x2060, 0xE3FC, 0xFC1F, 0x79B1, 0xB1C8, 0xB65B, 0x5BED ]
            , [ 0xD46A, 0x6ABE, 0x8DCB, 0xCB46, 0x67BE, 0xBED9, 0x7239, 0x394B ]
            , [ 0x944A, 0x4ADE, 0x984C, 0x4CD4, 0xB058, 0x58E8, 0x85CF, 0xCF4A ]
            , [ 0xBBD0, 0xD06B, 0xC5EF, 0xEF2A, 0x4FAA, 0xAAE5, 0xEDFB, 0xFB16 ]
            , [ 0x8643, 0x43C5, 0x9A4D, 0x4DD7, 0x6633, 0x3355, 0x1185, 0x8594 ]
            , [ 0x8A45, 0x45CF, 0xE9F9, 0xF910, 0x0402, 0x0206, 0xFE7F, 0x7F81 ]
            , [ 0xA050, 0x50F0, 0x783C, 0x3C44, 0x259F, 0x9FBA, 0x4BA8, 0xA8E3 ]
            , [ 0xA251, 0x51F3, 0x5DA3, 0xA3FE, 0x8040, 0x40C0, 0x058F, 0x8F8A ]
            , [ 0x3F92, 0x92AD, 0x219D, 0x9DBC, 0x7038, 0x3848, 0xF1F5, 0xF504 ]
            , [ 0x63BC, 0xBCDF, 0x77B6, 0xB6C1, 0xAFDA, 0xDA75, 0x4221, 0x2163 ]
            , [ 0x2010, 0x1030, 0xE5FF, 0xFF1A, 0xFDF3, 0xF30E, 0xBFD2, 0xD26D ]
            , [ 0x81CD, 0xCD4C, 0x180C, 0x0C14, 0x2613, 0x1335, 0xC3EC, 0xEC2F ]
            , [ 0xBE5F, 0x5FE1, 0x3597, 0x97A2, 0x8844, 0x44CC, 0x2E17, 0x1739 ]
            , [ 0x93C4, 0xC457, 0x55A7, 0xA7F2, 0xFC7E, 0x7E82, 0x7A3D, 0x3D47 ]
            , [ 0xC864, 0x64AC, 0xBA5D, 0x5DE7, 0x3219, 0x192B, 0xE673, 0x7395 ]
            , [ 0xC060, 0x60A0, 0x1981, 0x8198, 0x9E4F, 0x4FD1, 0xA3DC, 0xDC7F ]
            , [ 0x4422, 0x2266, 0x542A, 0x2A7E, 0x3B90, 0x90AB, 0x0B88, 0x8883 ]
            , [ 0x8C46, 0x46CA, 0xC7EE, 0xEE29, 0x6BB8, 0xB8D3, 0x2814, 0x143C ]
            , [ 0xA7DE, 0xDE79, 0xBC5E, 0x5EE2, 0x160B, 0x0B1D, 0xADDB, 0xDB76 ]
            , [ 0xDBE0, 0xE03B, 0x6432, 0x3256, 0x743A, 0x3A4E, 0x140A, 0x0A1E ]
            , [ 0x9249, 0x49DB, 0x0C06, 0x060A, 0x4824, 0x246C, 0xB85C, 0x5CE4 ]
            , [ 0x9FC2, 0xC25D, 0xBDD3, 0xD36E, 0x43AC, 0xACEF, 0xC462, 0x62A6 ]
            , [ 0x3991, 0x91A8, 0x3195, 0x95A4, 0xD3E4, 0xE437, 0xF279, 0x798B ]
            , [ 0xD5E7, 0xE732, 0x8BC8, 0xC843, 0x6E37, 0x3759, 0xDA6D, 0x6DB7 ]
            , [ 0x018D, 0x8D8C, 0xB1D5, 0xD564, 0x9C4E, 0x4ED2, 0x49A9, 0xA9E0 ]
            , [ 0xD86C, 0x6CB4, 0xAC56, 0x56FA, 0xF3F4, 0xF407, 0xCFEA, 0xEA25 ]
            , [ 0xCA65, 0x65AF, 0xF47A, 0x7A8E, 0x47AE, 0xAEE9, 0x1008, 0x0818 ]
            , [ 0x6FBA, 0xBAD5, 0xF078, 0x7888, 0x4A25, 0x256F, 0x5C2E, 0x2E72 ]
            , [ 0x381C, 0x1C24, 0x57A6, 0xA6F1, 0x73B4, 0xB4C7, 0x97C6, 0xC651 ]
            , [ 0xCBE8, 0xE823, 0xA1DD, 0xDD7C, 0xE874, 0x749C, 0x3E1F, 0x1F21 ]
            , [ 0x964B, 0x4BDD, 0x61BD, 0xBDDC, 0x0D8B, 0x8B86, 0x0F8A, 0x8A85 ]
            , [ 0xE070, 0x7090, 0x7C3E, 0x3E42, 0x71B5, 0xB5C4, 0xCC66, 0x66AA ]
            , [ 0x9048, 0x48D8, 0x0603, 0x0305, 0xF7F6, 0xF601, 0x1C0E, 0x0E12 ]
            , [ 0xC261, 0x61A3, 0x6A35, 0x355F, 0xAE57, 0x57F9, 0x69B9, 0xB9D0 ]
            , [ 0x1786, 0x8691, 0x99C1, 0xC158, 0x3A1D, 0x1D27, 0x279E, 0x9EB9 ]
            , [ 0xD9E1, 0xE138, 0xEBF8, 0xF813, 0x2B98, 0x98B3, 0x2211, 0x1133 ]
            , [ 0xD269, 0x69BB, 0xA9D9, 0xD970, 0x078E, 0x8E89, 0x3394, 0x94A7 ]
            , [ 0x2D9B, 0x9BB6, 0x3C1E, 0x1E22, 0x1587, 0x8792, 0xC9E9, 0xE920 ]
            , [ 0x87CE, 0xCE49, 0xAA55, 0x55FF, 0x5028, 0x2878, 0xA5DF, 0xDF7A ]
            , [ 0x038C, 0x8C8F, 0x59A1, 0xA1F8, 0x0989, 0x8980, 0x1A0D, 0x0D17 ]
            , [ 0x65BF, 0xBFDA, 0xD7E6, 0xE631, 0x8442, 0x42C6, 0xD068, 0x68B8 ]
            , [ 0x8241, 0x41C3, 0x2999, 0x99B0, 0x5A2D, 0x2D77, 0x1E0F, 0x0F11 ]
            , [ 0x7BB0, 0xB0CB, 0xA854, 0x54FC, 0x6DBB, 0xBBD6, 0x2C16, 0x163A ]
            ]


rt0_ : Array Int
rt0_ =
    fromList <|
        List.concat
            [ [ 0x51F4, 0xA750, 0x7E41, 0x6553, 0x1A17, 0xA4C3, 0x3A27, 0x5E96 ]
            , [ 0x3BAB, 0x6BCB, 0x1F9D, 0x45F1, 0xACFA, 0x58AB, 0x4BE3, 0x0393 ]
            , [ 0x2030, 0xFA55, 0xAD76, 0x6DF6, 0x88CC, 0x7691, 0xF502, 0x4C25 ]
            , [ 0x4FE5, 0xD7FC, 0xC52A, 0xCBD7, 0x2635, 0x4480, 0xB562, 0xA38F ]
            , [ 0xDEB1, 0x5A49, 0x25BA, 0x1B67, 0x45EA, 0x0E98, 0x5DFE, 0xC0E1 ]
            , [ 0xC32F, 0x7502, 0x814C, 0xF012, 0x8D46, 0x97A3, 0x6BD3, 0xF9C6 ]
            , [ 0x038F, 0x5FE7, 0x1592, 0x9C95, 0xBF6D, 0x7AEB, 0x9552, 0x59DA ]
            , [ 0xD4BE, 0x832D, 0x5874, 0x21D3, 0x49E0, 0x6929, 0x8EC9, 0xC844 ]
            , [ 0x75C2, 0x896A, 0xF48E, 0x7978, 0x9958, 0x3E6B, 0x27B9, 0x71DD ]
            , [ 0xBEE1, 0x4FB6, 0xF088, 0xAD17, 0xC920, 0xAC66, 0x7DCE, 0x3AB4 ]
            , [ 0x63DF, 0x4A18, 0xE51A, 0x3182, 0x9751, 0x3360, 0x6253, 0x7F45 ]
            , [ 0xB164, 0x77E0, 0xBB6B, 0xAE84, 0xFE81, 0xA01C, 0xF908, 0x2B94 ]
            , [ 0x7048, 0x6858, 0x8F45, 0xFD19, 0x94DE, 0x6C87, 0x527B, 0xF8B7 ]
            , [ 0xAB73, 0xD323, 0x724B, 0x02E2, 0xE31F, 0x8F57, 0x6655, 0xAB2A ]
            , [ 0xB2EB, 0x2807, 0x2FB5, 0xC203, 0x86C5, 0x7B9A, 0xD337, 0x08A5 ]
            , [ 0x3028, 0x87F2, 0x23BF, 0xA5B2, 0x0203, 0x6ABA, 0xED16, 0x825C ]
            , [ 0x8ACF, 0x1C2B, 0xA779, 0xB492, 0xF307, 0xF2F0, 0x4E69, 0xE2A1 ]
            , [ 0x65DA, 0xF4CD, 0x0605, 0xBED5, 0xD134, 0x621F, 0xC4A6, 0xFE8A ]
            , [ 0x342E, 0x539D, 0xA2F3, 0x55A0, 0x058A, 0xE132, 0xA4F6, 0xEB75 ]
            , [ 0x0B83, 0xEC39, 0x4060, 0xEFAA, 0x5E71, 0x9F06, 0xBD6E, 0x1051 ]
            , [ 0x3E21, 0x8AF9, 0x96DD, 0x063D, 0xDD3E, 0x05AE, 0x4DE6, 0xBD46 ]
            , [ 0x9154, 0x8DB5, 0x71C4, 0x5D05, 0x0406, 0xD46F, 0x6050, 0x15FF ]
            , [ 0x1998, 0xFB24, 0xD6BD, 0xE997, 0x8940, 0x43CC, 0x67D9, 0x9E77 ]
            , [ 0xB0E8, 0x42BD, 0x0789, 0x8B88, 0xE719, 0x5B38, 0x79C8, 0xEEDB ]
            , [ 0xA17C, 0x0A47, 0x7C42, 0x0FE9, 0xF884, 0x1EC9, 0x00, 0x00 ]
            , [ 0x0980, 0x8683, 0x322B, 0xED48, 0x1E11, 0x70AC, 0x6C5A, 0x724E ]
            , [ 0xFD0E, 0xFFFB, 0x0F85, 0x3856, 0x3DAE, 0xD51E, 0x362D, 0x3927 ]
            , [ 0x0A0F, 0xD964, 0x685C, 0xA621, 0x9B5B, 0x54D1, 0x2436, 0x2E3A ]
            , [ 0x0C0A, 0x67B1, 0x9357, 0xE70F, 0xB4EE, 0x96D2, 0x1B9B, 0x919E ]
            , [ 0x80C0, 0xC54F, 0x61DC, 0x20A2, 0x5A77, 0x4B69, 0x1C12, 0x1A16 ]
            , [ 0xE293, 0xBA0A, 0xC0A0, 0x2AE5, 0x3C22, 0xE043, 0x121B, 0x171D ]
            , [ 0x0E09, 0x0D0B, 0xF28B, 0xC7AD, 0x2DB6, 0xA8B9, 0x141E, 0xA9C8 ]
            , [ 0x57F1, 0x1985, 0xAF75, 0x074C, 0xEE99, 0xDDBB, 0xA37F, 0x60FD ]
            , [ 0xF701, 0x269F, 0x5C72, 0xF5BC, 0x4466, 0x3BC5, 0x5BFB, 0x7E34 ]
            , [ 0x8B43, 0x2976, 0xCB23, 0xC6DC, 0xB6ED, 0xFC68, 0xB8E4, 0xF163 ]
            , [ 0xD731, 0xDCCA, 0x4263, 0x8510, 0x1397, 0x2240, 0x84C6, 0x1120 ]
            , [ 0x854A, 0x247D, 0xD2BB, 0x3DF8, 0xAEF9, 0x3211, 0xC729, 0xA16D ]
            , [ 0x1D9E, 0x2F4B, 0xDCB2, 0x30F3, 0x0D86, 0x52EC, 0x77C1, 0xE3D0 ]
            , [ 0x2BB3, 0x166C, 0xA970, 0xB999, 0x1194, 0x48FA, 0x47E9, 0x6422 ]
            , [ 0xA8FC, 0x8CC4, 0xA0F0, 0x3F1A, 0x567D, 0x2CD8, 0x2233, 0x90EF ]
            , [ 0x8749, 0x4EC7, 0xD938, 0xD1C1, 0x8CCA, 0xA2FE, 0x98D4, 0x0B36 ]
            , [ 0xA6F5, 0x81CF, 0xA57A, 0xDE28, 0xDAB7, 0x8E26, 0x3FAD, 0xBFA4 ]
            , [ 0x2C3A, 0x9DE4, 0x5078, 0x920D, 0x6A5F, 0xCC9B, 0x547E, 0x4662 ]
            , [ 0xF68D, 0x13C2, 0x90D8, 0xB8E8, 0x2E39, 0xF75E, 0x82C3, 0xAFF5 ]
            , [ 0x9F5D, 0x80BE, 0x69D0, 0x937C, 0x6FD5, 0x2DA9, 0xCF25, 0x12B3 ]
            , [ 0xC8AC, 0x993B, 0x1018, 0x7DA7, 0xE89C, 0x636E, 0xDB3B, 0xBB7B ]
            , [ 0xCD26, 0x7809, 0x6E59, 0x18F4, 0xEC9A, 0xB701, 0x834F, 0x9AA8 ]
            , [ 0xE695, 0x6E65, 0xAAFF, 0xE67E, 0x21BC, 0xCF08, 0xEF15, 0xE8E6 ]
            , [ 0xBAE7, 0x9BD9, 0x4A6F, 0x36CE, 0xEA9F, 0x09D4, 0x29B0, 0x7CD6 ]
            , [ 0x31A4, 0xB2AF, 0x2A3F, 0x2331, 0xC6A5, 0x9430, 0x35A2, 0x66C0 ]
            , [ 0x744E, 0xBC37, 0xFC82, 0xCAA6, 0xE090, 0xD0B0, 0x33A7, 0xD815 ]
            , [ 0xF104, 0x984A, 0x41EC, 0xDAF7, 0x7FCD, 0x500E, 0x1791, 0xF62F ]
            , [ 0x764D, 0xD68D, 0x43EF, 0xB04D, 0xCCAA, 0x4D54, 0xE496, 0x04DF ]
            , [ 0x9ED1, 0xB5E3, 0x4C6A, 0x881B, 0xC12C, 0x1FB8, 0x4665, 0x517F ]
            , [ 0x9D5E, 0xEA04, 0x018C, 0x355D, 0xFA87, 0x7473, 0xFB0B, 0x412E ]
            , [ 0xB367, 0x1D5A, 0x92DB, 0xD252, 0xE910, 0x5633, 0x6DD6, 0x4713 ]
            , [ 0x9AD7, 0x618C, 0x37A1, 0x0C7A, 0x59F8, 0x148E, 0xEB13, 0x3C89 ]
            , [ 0xCEA9, 0x27EE, 0xB761, 0xC935, 0xE11C, 0xE5ED, 0x7A47, 0xB13C ]
            , [ 0x9CD2, 0xDF59, 0x55F2, 0x733F, 0x1814, 0xCE79, 0x73C7, 0x37BF ]
            , [ 0x53F7, 0xCDEA, 0x5FFD, 0xAA5B, 0xDF3D, 0x6F14, 0x7844, 0xDB86 ]
            , [ 0xCAAF, 0xF381, 0xB968, 0xC43E, 0x3824, 0x342C, 0xC2A3, 0x405F ]
            , [ 0x161D, 0xC372, 0xBCE2, 0x250C, 0x283C, 0x498B, 0xFF0D, 0x9541 ]
            , [ 0x39A8, 0x0171, 0x080C, 0xB3DE, 0xD8B4, 0xE49C, 0x6456, 0xC190 ]
            , [ 0x7BCB, 0x8461, 0xD532, 0xB670, 0x486C, 0x5C74, 0xD0B8, 0x5742 ]
            ]


{-| Generate the remaining forward and reverse tables based on the above
-}
ft1_ =
    arrayRotatePairsRight ft0_


ft2_ =
    arrayRotatePairsRight ft1_


ft3_ =
    arrayRotatePairsRight ft2_


rt1_ =
    arrayRotatePairsRight rt0_


rt2_ =
    arrayRotatePairsRight rt1_


rt3_ =
    arrayRotatePairsRight rt2_


{-| Reverse key expansion tables.
-}
kt0_ =
    genKTable rt0_


kt1_ =
    genKTable rt1_


kt2_ =
    genKTable rt2_


kt3_ =
    genKTable rt3_
