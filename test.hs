{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RebindableSyntax  #-}

-- Compile with: fay --package fay-simplejson test.hs --pretty

module Test (main) where

import           Data.Text  (Text, fromString)
import           Prelude
import           SimpleJSON

data SubTest = SubTest { xsKey :: Text, xsValue :: Text }

subTestDecoder :: Decoder
subTestDecoder = withDecoder "SubTest" [ customDecoderRule "xsKey" "key",
                                         customDecoderRule "xsValue" "value" ]
subTestEncoder :: Encoder
subTestEncoder = withEncoder [ customEncoderRule "key" "xsKey",
                               customEncoderRule "value" "xsValue" ]

data Test = Test { xKey :: Text, xValue :: Text, subTest :: SubTest }

testDecoder :: Decoder
testDecoder = withDecoder "Text" [ customDecoderRule "xkey" "key",
                                   customDecoderRule "xValue" "value",
                                   decoderRule       "subTest" "subTest" subTestDecoder,
                                   listDecoderRule   "subTestList" "subTestList" subTestDecoder ]

testEncoder :: Encoder
testEncoder = withEncoder [ customEncoderRule "key" "xKey",
                            customEncoderRule "value" "xValue",
                            encoderRule       "subTest" "subTest" subTestEncoder,
                            listEncoderRule   "subTestList" "subTestList" subTestEncoder ]

main :: Fay ()
main = do
  let test = decode "[{\"key\": \"test_key\", \"value\": \"test_value\", \"subTest\": {\"key\": \"test_key\", \"value\": \"test_value\"}, \"subTestList\": [{\"key\": \"test_key1\", \"value\": \"test_value1\"}]}]" testDecoder :: [Test]

  print test

  print $ encode test testEncoder
