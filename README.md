[![elm-package](https://img.shields.io/badge/elm-1.0.0-blue.svg)](http://package.elm-lang.org/packages/billstclair/elm-aes/latest)
[![Build Status](https://travis-ci.org/billstclair/elm-aes.svg?branch=master)](https://travis-ci.org/billstclair/elm-aes)

A pure Elm implementation of Rijndael, the Advanced Encryption Standard (AES). An Elm rendering of [cl-cryto](https://github.com/billstclair/cl-crypto)'s [aes16.lisp](https://github.com/billstclair/cl-crypto/blob/master/source/aes16.lisp).

This is a work in progress. Block encryption and decryption work. Still must do salting, block chaining, and conversion to and from strings.

# Example

The [`example`](https://github.com/billstclair/elm-aes/tree/master/example) directory will have some sample code, with a simple user interface.
