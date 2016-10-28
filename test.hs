{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RebindableSyntax  #-}

-- Compile with: fay --package fay-simplejson test.hs --pretty

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
