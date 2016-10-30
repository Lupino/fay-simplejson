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
subTestDecoder = withDecoder "SubTest" [ rawRule "xsKey" "key",
                                         rawRule "xsValue" "value" ]
subTestEncoder :: Parser
subTestEncoder = withEncoder [ rawRule "key" "xsKey",
                               rawRule "value" "xsValue" ]

data Test = Test { xKey          :: Text,
                   xValue        :: Text,
                   subTest       :: SubTest,
                   subTestList   :: [SubTest],
                   maybeTest     :: Maybe SubTest,
                   maybeTestList :: Maybe [SubTest]
                   }

testDecoder :: Parser
testDecoder = withDecoder "Text" [ rawRule  "xkey" "key",
                                   rawRule  "xValue" "value",
                                   rule     subTestDecoder "subTest" "subTest",
                                   listRule subTestDecoder "subTestList" "subTestList",
                                   rule     (maybeParser subTestDecoder) "maybeTest" "maybeTest",
                                   rule     (maybeParser $ listParser subTestDecoder) "maybeTestList" "maybeTestList"
                                   ]

testEncoder :: Parser
testEncoder = withEncoder [ rawRule  "key" "xKey",
                            rawRule  "value" "xValue",
                            rule     subTestEncoder "subTest" "subTest",
                            listRule subTestEncoder "subTestList" "subTestList",
                            rule     (fromMaybeParser subTestEncoder) "maybeTest" "maybeTest",
                            rule     (fromMaybeParser $ listParser subTestEncoder) "maybeTestList" "maybeTestList"
                            ]

main :: Fay ()
main = do
  let test = decode "[{\"key\": \"test_key\", \"value\": \"test_value\", \"subTest\": {\"key\": \"test_key\", \"value\": \"test_value\"}, \"subTestList\": [{\"key\": \"test_key1\", \"value\": \"test_value1\"}]}]" testDecoder :: [Test]

  print test

  print $ encode test testEncoder
