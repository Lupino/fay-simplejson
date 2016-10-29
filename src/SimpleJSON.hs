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
    rawParser,
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

newtype Parser = Parser (Value -> Fay Value)

data Rule = CustomRule Text Text
          | Rule       Text Text Parser
          | ListRule   Text Text Parser

customRule :: Text -> Text -> Rule
customRule = CustomRule

rule :: Text -> Text -> Parser -> Rule
rule = Rule

listRule :: Text -> Text -> Parser -> Rule
listRule = ListRule

runRule :: Rule -> Value -> Value -> Fay Value
runRule (CustomRule ref key) v0 v1 = set v0 ref =<< get v1 key
runRule (Rule ref key p) v0 v1     = set v0 ref =<< runParser p =<< get v1 key
runRule (ListRule ref key p) v0 v1 = set v0 ref =<< runListParser p =<< get v1 key

toParser :: (Value -> Fay Value) -> Parser
toParser = Parser

rawParser :: Parser
rawParser = toParser return

runParser :: Parser -> Value -> Fay Value
runParser (Parser f) v = f v

runListParser :: Parser -> Value -> Fay Value
runListParser (Parser f) v = do
  vs <- toList v
  parsed <- mapM f vs
  return $ unsafeCoerce parsed

toList :: Value -> Fay [Value]
toList = ffi "(function(v){ if (Array.isArray(v)){ return v; } else if (v) { return [v]; } else { return [] } })(%1)"

set :: Value -> Text -> b -> Fay Value
set = ffi "(function(obj, key, val) { obj[key] = val; return obj; })(%1, %2, %3)"

get :: Value -> Text -> Fay b
get = ffi "(function(v, k) { if (v) {return v[k]; } else { return '' }})(%1, %2)"

isList :: Value -> Bool
isList = ffi "Array.isArray(%1)"

decodeRaw :: Text -> Fay Value
decodeRaw = ffi "(function(v) { v = JSON.parse(v); return v })(%1)"

encodeRaw :: Value -> Fay Text
encodeRaw = ffi "JSON.stringify(%1)"

decode :: Text -> Parser -> a
decode txt p = unsafeCoerce (unsafePerformFay $ runP p v)
  where v = unsafePerformFay $ decodeRaw txt
        runP = if isList v then runListParser else runParser

encode :: a -> Parser -> Text
encode obj p = unsafePerformFay (encodeRaw =<< runP p v)
  where v = unsafeCoerce obj :: Value
        runP = if isList v then runListParser else runParser

newValue :: Fay Value
newValue = ffi "{}"

withDecoder :: Text -> [Rule] -> Parser
withDecoder ins rules = toParser (go (unsafePerformFay $ set (unsafePerformFay newValue) "instance" ins) rules)
  where go :: Value -> [Rule] -> Value -> Fay Value
        go obj (x:xs) v = do
          newObj <- runRule x obj v
          go newObj xs v
        go obj [] _     = return obj

withEncoder :: [Rule] -> Parser
withEncoder rules = toParser (go (unsafePerformFay newValue) rules)
  where go :: Value -> [Rule] -> Value -> Fay Value
        go v (x:xs) obj = do
          newV <- runRule x v obj
          go newV xs obj
        go v [] _       = return v
