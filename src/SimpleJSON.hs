{-# OPTIONS -fno-warn-redundant-constraints #-}
{-# LANGUAGE EmptyDataDecls    #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RebindableSyntax  #-}

module SimpleJSON
  (
    Value,
    Parser,
    toParser,
    Rule,
    customRule,
    rule,
    listRule,
    runParser,
    runListParser,
    withDecoder,
    withEncoder,
    decode,
    encode,
    decodeRaw,
    encodeRaw,
  ) where

import           Data.Text     (Text, fromString)
import           Fay.Unsafe    (unsafePerformFay)
import           FFI           (ffi)
import           Prelude
import           Unsafe.Coerce (unsafeCoerce)
data Value

newtype Parser = Parser (Value -> Value)

data Rule = CustomRule Text Text
          | Rule       Text Text Parser
          | ListRule   Text Text Parser

customRule :: Text -> Text -> Rule
customRule = CustomRule

rule :: Text -> Text -> Parser -> Rule
rule = Rule

listRule :: Text -> Text -> Parser -> Rule
listRule = ListRule

runRule :: Rule -> Value -> Value -> Value
runRule (CustomRule ref key) v0 v1 = set v0 ref $ get v1 key
runRule (Rule ref key p) v0 v1     = set v0 ref $ runParser p (get v1 key)
runRule (ListRule ref key p) v0 v1 = set v0 ref $ runListParser p (get v1 key)

toParser :: (Value -> Value) -> Parser
toParser = Parser

runParser :: Parser -> Value -> Value
runParser (Parser f) v = f v

runListParser :: Parser -> Value -> Value
runListParser (Parser f) v = unsafeCoerce $ map f $ unsafeCoerce v

set :: Value -> Text -> b -> Value
set = ffi "(function(obj, key, val) { obj[key] = val; return obj; })(%1, %2, %3)"

get :: Value -> Text -> b
get = ffi "(function(v, k) { if (v) {return v[k]; } else { return '' }})(%1, %2)"

isList :: Value -> Bool
isList = ffi "Array.isArray(%1)"

decodeRaw :: Text -> Value
decodeRaw = ffi "JSON.parse(%1)"

encodeRaw :: Value -> Text
encodeRaw = ffi "JSON.stringify(%1)"

decode :: Text -> Parser -> a
decode txt p = unsafeCoerce $ runP p v
  where v = decodeRaw txt
        runP = if isList v then runListParser else runParser

encode :: a -> Parser -> Text
encode obj p = encodeRaw $ runP p v
  where v = unsafeCoerce obj :: Value
        runP = if isList v then runListParser else runParser

newValue :: Fay Value
newValue = ffi "{}"

withDecoder :: Text -> [Rule] -> Parser
withDecoder ins rules = toParser (go (set (unsafePerformFay newValue) "instance" ins) rules)
  where go :: Value -> [Rule] -> Value -> Value
        go obj (x:xs) v = runRule x (go obj xs v) v
        go obj [] _     = obj

withEncoder :: [Rule] -> Parser
withEncoder rules = toParser (go (unsafePerformFay newValue) rules)
  where go :: Value -> [Rule] -> Value -> Value
        go v (x:xs) obj = runRule x (go v xs obj) obj
        go v [] _       = v
