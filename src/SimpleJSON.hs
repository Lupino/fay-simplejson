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
    toMaybeParser,
    fromMaybeParser,
    listParser,
    (>>>),
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

(>>>) :: Parser -> Parser -> Parser
(Parser f) >>> (Parser g) = toParser $ \v -> g =<< f v

toMaybeParser :: Parser
toMaybeParser = toParser toMaybe
  where toMaybe :: Value -> Fay Value
        toMaybe = ffi "(function(v) {\
                       \  if (v) {\
                       \    return { _instance: 'Just', slot1: v };\
                       \  } else {\
                       \    return { _instance: 'Nothing' };\
                       \  }\
                       \})(%1)"

fromMaybeParser :: Parser
fromMaybeParser = toParser fromMaybe'
  where fromMaybe' :: Value -> Fay Value
        fromMaybe' = ffi "(function(v) {\
                          \  if (v._instance === 'Just') {\
                          \    return v.slot1;\
                          \  } else {\
                          \    return null;\
                          \  }\
                          \})(%1)"

listParser :: Parser -> Parser
listParser p = toParser $ runListParser p

runParser :: Parser -> Value -> Fay Value
runParser (Parser f) v = f v

runListParser :: Parser -> Value -> Fay Value
runListParser p v = do
  parsed <- mapM (runParser p) =<< toList v
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
decodeRaw = ffi "JSON.parse(%1)"

encodeRaw :: Value -> Fay Text
encodeRaw = ffi "JSON.stringify(%1)"

decode :: Text -> Parser -> a
decode txt p = unsafePerformFay $ fixedInstance =<< runP p v
  where v = unsafePerformFay $ decodeRaw txt
        runP = if isList v then runListParser else runParser

fixedInstance :: Value -> Fay a
fixedInstance = ffi "(function(v){\
                  \  function fixedObject(v) {\
                  \    var o = {};\
                  \    for (var k in v) {\
                  \      if (k === '_instance') {\
                  \        o[k.substr(1)] = v[k];\
                  \        continue;\
                  \      }\
                  \      if (k === 'instance') {\
                  \        o['_' + k] = v[k];\
                  \        continue;\
                  \      }\
                  \      o[k] = fixed(v[k]);\
                  \    }\
                  \    return o;\
                  \  }\
                  \  function fixedArray(v) {\
                  \    return v.map(fixed);\
                  \  }\
                  \  function fixed(v) {\
                  \    if (Array.isArray(v)) {\
                  \      return fixedArray(v);\
                  \    } else if (typeof v === 'object') {\
                  \      return fixedObject(v);\
                  \    } else {\
                  \      return v\
                  \    }\
                  \  }\
                  \  return fixed(v);\
                  \})(%1)"

encode :: a -> Parser -> Text
encode obj p = unsafePerformFay (encodeRaw =<< runP p =<< fixedInstance v)
  where v = unsafeCoerce obj :: Value
        runP = if isList v then runListParser else runParser

newValue :: Fay Value
newValue = ffi "{}"

doParser :: Fay Value -> [Rule] -> Value -> Fay Value
doParser obj rules ref = do
  o <- obj
  go ref rules o
  where go :: Value -> [Rule] -> Value -> Fay Value
        go v (x:xs) o = go v xs =<< runRule x o v
        go _ [] o     = return o

withDecoder :: Text -> [Rule] -> Parser
withDecoder ins rules = toParser (doParser newV rules)
  where newV :: Fay Value
        newV = do
          v <- newValue
          set v "_instance" ins


withEncoder :: [Rule] -> Parser
withEncoder rules = toParser (doParser newValue rules)
