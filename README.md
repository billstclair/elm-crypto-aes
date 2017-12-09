[![elm-package](https://img.shields.io/badge/elm-1.0.5-blue.svg)](http://package.elm-lang.org/packages/billstclair/elm-crypto-aes/latest)
[![Build Status](https://travis-ci.org/billstclair/elm-crypto-aes.svg?branch=master)](https://travis-ci.org/billstclair/elm-crypto-aes)

A pure Elm implementation of Rijndael, the Advanced Encryption Standard (AES). An Elm rendering of [cl-cryto](https://github.com/billstclair/cl-crypto)'s [aes16.lisp](https://github.com/billstclair/cl-crypto/blob/master/source/aes16.lisp).

This package does low-level encryption/decryption of 16-byte blocks only. For actual application use, you'll want a higher-level package, using strings as passphrases and block chaining to encrypt longer text, e.g. [`billstclair/elm-crypto-string`](http://package.elm-lang.org/packages/billstclair/elm-crypto-string/latest)

Bill St. Clair<br/>
5 December, 2017

