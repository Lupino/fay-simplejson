{-# OPTIONS -fno-warn-redundant-constraints #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RebindableSyntax  #-}

module SimpleJSON
  (
    Value,
    Decoder,
    Encoder,
    toDecoder,
    toEncoder,
    fromJSON,
    toJSON,
    withDecoder,
    withEncoder,
    decode,
    encode,
    decodeRaw,
    encodeRaw,
    JSONSetter,
    JSONGetter,
    KeyRef,
    KeyRefMap,
    (.>),
    (.<),
    get,
    set
  ) where

import           Data.Text (Text, fromString)
import           FFI       (ffi)
import           Prelude

data Value

class JSONGetter v
instance JSONGetter Value

class JSONSetter v
instance JSONSetter Value

newtype Decoder a = Decoder (Value -> a)
newtype Encoder a = Encoder (a -> Value)

type KeyRef = (Text,Text)
type KeyRefMap   = [KeyRef]

(.>) :: Text -> Text -> KeyRef
a .> b = (a, b)

(.<) :: Text -> Text -> KeyRef
a .< b = (b, a)

toDecoder :: (Value -> a) -> Decoder a
toDecoder = Decoder

toEncoder :: (a -> Value) -> Encoder a
toEncoder = Encoder

fromJSON :: Value -> Decoder a -> a
fromJSON v (Decoder f) = f v

toJSON :: a -> Encoder a -> Value
toJSON v (Encoder f) = f v

set :: JSONSetter a => a -> Text -> b -> a
set = ffi "(function(obj, key, val) { obj[key] = val; return obj; })(%1, %2, %3)"

get :: JSONGetter a => a -> Text -> b
get = ffi "%1[%2]"

decodeRaw :: Text -> Value
decodeRaw = ffi "JSON.parse(%1)"

encodeRaw :: Value -> Text
encodeRaw = ffi "JSON.stringify(%1)"

decode :: JSONSetter a => Text -> Decoder a -> a
decode txt = fromJSON (decodeRaw txt)

encode :: JSONGetter a => a -> Encoder a -> Text
encode obj up = encodeRaw $ toJSON obj up

newValue :: Value
newValue = ffi "{}"

newObject :: JSONSetter a => Text -> a
newObject = ffi "{instance: %1}"

withDecoder :: JSONSetter a => Text -> KeyRefMap -> Decoder a
withDecoder ins params = toDecoder (go (newObject ins) params)
  where go :: JSONSetter a => a -> KeyRefMap -> Value -> a
        go obj ((ref, key):xs) v = go (set obj ref $ get v key) xs v
        go obj [] _              = obj

withEncoder :: JSONGetter a => KeyRefMap -> Encoder a
withEncoder params = toEncoder (go newValue params)
  where go :: JSONGetter a => Value -> KeyRefMap -> a -> Value
        go v ((ref,key):xs) obj = go (set v key $ get obj ref) xs obj
        go v [] _               = v
