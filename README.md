Fay SimpleJSON
==============

SimpleJSON library for Fay.

```haskell
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RebindableSyntax  #-}

module Test (main) where

import           Data.Text  (Text, fromString)
import           Prelude
import           SimpleJSON

data Test = Test { xKey :: Text, xValue :: Text }
instance JSONSetter Test
instance JSONGetter Test

decoder :: Decoder Test
decoder = withDecoder "Test" ["xKey" .> "key", "xValue" .> "value"]

encoder :: Encoder TestJSON
encoder = withEncoder ["key" .< "xKey", "value" .< "xValue"]

main :: Fay ()
main = do
  let test = decode "{\"key\": \"test_key\", \"value\": \"test_value\"}" decoder
  print test
  print $ encode test encoder
```

Usage
-----


Install:
```bash
cabal install fay-simplejson
```

Compile your file:

```bash
fay --package fay-simplejson MyFile.hs
```
