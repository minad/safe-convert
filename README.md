# safe-convert: Safe type conversions

[![Hackage](https://img.shields.io/hackage/v/safe-convert.svg)](https://hackage.haskell.org/package/safe-convert)
[![Build Status](https://secure.travis-ci.org/minad/safe-convert.png?branch=master)](http://travis-ci.org/minad/safe-convert)

This package provides the Safe.Convert class for safe conversions. While not being the best idea for type inference, you get the additional
guarantee of safety. If there is a Safe.Convert instance - you are safe and the conversion is lossless (in contrast to functions like fromIntegral).
Additionally safe string decoding is provided.
