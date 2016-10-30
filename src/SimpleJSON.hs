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
    maybeParser,
    fromMaybeParser,
    listParser,
    (>>>),
    Rule,
    rawRule,
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

data Rule = Rule Parser Text Text

rule :: Parser -> Text -> Text -> Rule
rule = Rule

rawRule :: Text -> Text -> Rule
rawRule = rule rawParser

listRule :: Parser -> Text -> Text -> Rule
listRule p = rule (listParser p)

runRule :: Rule -> Value -> Value -> Fay Value
runRule (Rule p ref key) v0 v1 = set v0 ref =<< runParser p =<< get v1 key

toParser :: (Value -> Fay Value) -> Parser
toParser = Parser

rawParser :: Parser
rawParser = toParser return

(>>>) :: Parser -> Parser -> Parser
(Parser f) >>> (Parser g) = toParser $ \v -> g =<< f v

nullValue :: Value -> Bool
nullValue = ffi "(function(v) {\
                 \  if (!v) {\
                 \    return true;\
                 \  }\
                 \  if (Array.isArray(v)) {\
                 \     return v.length === 0;\
                 \  } else if (typeof v === 'object') {\
                 \    for (var i in v) {\
                 \      return false;\
                 \    }\
                 \    return true;\
                 \  }\
                 \  return false;\
                 \})"

maybeParser :: Parser -> Parser
maybeParser p = toParser toMaybe
  where toMaybe :: Value -> Fay Value
        toMaybe v | nullValue v = nothing
                  | otherwise   = just v

          where nothing :: Fay Value
                nothing = newValue' "Nothing"
                just :: Value -> Fay Value
                just v' = do o <- newValue' "Just"
                             set o "slot1" =<< runParser p v'

fromMaybeParser :: Parser -> Parser
fromMaybeParser p = toParser fromMaybe' >>> p
  where fromMaybe' :: Value -> Fay Value
        fromMaybe' = ffi "(function(v) {\
                          \  if (v._instance === 'Just') {\
                          \    return v.slot1;\
                          \  } else {\
                          \    return '';\
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

newValue' :: Text -> Fay Value
newValue' = ffi "{ _instance: %1 }"

doParser :: Fay Value -> [Rule] -> Value -> Fay Value
doParser obj rules ref = go ref rules =<< obj
  where go :: Value -> [Rule] -> Value -> Fay Value
        go v (x:xs) o = go v xs =<< runRule x o v
        go _ [] o     = return o

withDecoder :: Text -> [Rule] -> Parser
withDecoder ins rules = toParser (doParser (newValue' ins) rules)

withEncoder :: [Rule] -> Parser
withEncoder rules = toParser (doParser newValue rules)
