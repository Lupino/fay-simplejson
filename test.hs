{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RebindableSyntax  #-}

-- Compile with: fay --package fay-simplejson test.hs --pretty

module Test (main) where

import           Data.Text  (Text, fromString)
import           Prelude
import           SimpleJSON

data SubTest = SubTest { xsKey :: Text, xsValue :: Text }

subTestDecoder :: Parser
subTestDecoder = withDecoder "SubTest" [ customRule "xsKey" "key",
                                         customRule "xsValue" "value" ]
subTestEncoder :: Parser
subTestEncoder = withEncoder [ customRule "key" "xsKey",
                               customRule "value" "xsValue" ]

data Test = Test { xKey :: Text, xValue :: Text, subTest :: SubTest }

testDecoder :: Parser
testDecoder = withDecoder "Text" [ customRule "xkey" "key",
                                   customRule "xValue" "value",
                                   rule       "subTest" "subTest" subTestDecoder,
                                   listRule   "subTestList" "subTestList" subTestDecoder ]

testEncoder :: Parser
testEncoder = withEncoder [ customRule "key" "xKey",
                            customRule "value" "xValue",
                            rule       "subTest" "subTest" subTestEncoder,
                            listRule   "subTestList" "subTestList" subTestEncoder ]

main :: Fay ()
main = do
  let test = decode "[{\"key\": \"test_key\", \"value\": \"test_value\", \"subTest\": {\"key\": \"test_key\", \"value\": \"test_value\"}, \"subTestList\": [{\"key\": \"test_key1\", \"value\": \"test_value1\"}]}]" testDecoder :: [Test]

  print test

  print $ encode test testEncoder
