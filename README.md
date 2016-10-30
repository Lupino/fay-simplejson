Fay SimpleJSON
==============

SimpleJSON library for Fay.

```haskell
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RebindableSyntax  #-}

-- Compile with: fay --package fay-simplejson test.hs --pretty

module Test (main) where

import           Data.Text  (Text, fromString)
import           Prelude
import           SimpleJSON

data Test = Test { xKey :: Text, xValue :: Text }

decoder :: Parser
decoder = withDecoder "Test" [ rawRule "xKey"   "key",
                               rawRule "xValue" "value" ]

encoder :: Parser
encoder = withEncoder [ rawRule "key"   "xKey",
                        rawRule "value" "xValue" ]

main :: Fay ()
main = do
  let test = decode "{\"key\": \"test_key\", \"value\": \"test_value\"}" decoder :: Test
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
