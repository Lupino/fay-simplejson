{-# OPTIONS -fno-warn-redundant-constraints #-}
{-# LANGUAGE EmptyDataDecls    #-}
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
    DecoderRule,
    customDecoderRule,
    decoderRule,
    listDecoderRule,
    EncoderRule,
    customEncoderRule,
    encoderRule,
    listEncoderRule,
    fromJSON,
    toJSON,
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

newtype Decoder = Decoder (Value -> Value)
newtype Encoder = Encoder (Value -> Value)

data DecoderRule = CustomDecoderRule Text Text
                 | DecoderRule       Text Text Decoder
                 | ListDecoderRule   Text Text Decoder

customDecoderRule :: Text -> Text -> DecoderRule
customDecoderRule = CustomDecoderRule

decoderRule :: Text -> Text -> Decoder -> DecoderRule
decoderRule = DecoderRule

listDecoderRule :: Text -> Text -> Decoder -> DecoderRule
listDecoderRule = ListDecoderRule

runDecoderRule :: DecoderRule -> Value -> Value -> Value
runDecoderRule (CustomDecoderRule ref key) v0 v1 = set v0 ref $ get v1 key
runDecoderRule (DecoderRule ref key dec) v0 v1 = set v0 ref $ fromJSON (get v1 key) dec
runDecoderRule (ListDecoderRule ref key dec) v0 v1 = set v0 ref $ fromJSONList (get v1 key) dec

data EncoderRule = CustomEncoderRule Text Text
                 | EncoderRule       Text Text Encoder
                 | ListEncoderRule   Text Text Encoder

customEncoderRule :: Text -> Text -> EncoderRule
customEncoderRule = CustomEncoderRule

encoderRule :: Text -> Text -> Encoder -> EncoderRule
encoderRule = EncoderRule

listEncoderRule :: Text -> Text -> Encoder -> EncoderRule
listEncoderRule = ListEncoderRule

runEncoderRule :: EncoderRule -> Value -> Value -> Value
runEncoderRule (CustomEncoderRule ref key) v0 v1 = set v0 ref $ get v1 key
runEncoderRule (EncoderRule ref key enc) v0 v1 = set v0 ref $ toJSON (get v1 key) enc
runEncoderRule (ListEncoderRule ref key enc) v0 v1 = set v0 ref $ toJSONList (get v1 key) enc

toDecoder :: (Value -> Value) -> Decoder
toDecoder = Decoder

toEncoder :: (Value -> Value) -> Encoder
toEncoder = Encoder

fromJSON :: Value -> Decoder -> Value
fromJSON v (Decoder f) = f v

fromJSONList :: Value -> Decoder -> Value
fromJSONList v (Decoder f) = unsafeCoerce $ map f $ unsafeCoerce v

toJSON :: Value -> Encoder -> Value
toJSON v (Encoder f) = f v

toJSONList :: Value -> Encoder -> Value
toJSONList v (Encoder f) = unsafeCoerce $ map f $ unsafeCoerce v


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

decode :: Text -> Decoder -> a
decode txt dec = unsafeCoerce $ p v dec
  where v = decodeRaw txt
        p = if isList v then fromJSONList else fromJSON

encode :: a -> Encoder -> Text
encode obj up = encodeRaw $ p v up
  where v = unsafeCoerce obj :: Value
        p = if isList v then toJSONList else toJSON

newValue :: Fay Value
newValue = ffi "{}"

withDecoder :: Text -> [DecoderRule] -> Decoder
withDecoder ins rules = toDecoder (go (set (unsafePerformFay newValue) "instance" ins) rules)
  where go :: Value -> [DecoderRule] -> Value -> Value
        go obj (x:xs) v = runDecoderRule x (go obj xs v) v
        go obj [] _     = obj

withEncoder :: [EncoderRule] -> Encoder
withEncoder rules = toEncoder (go (unsafePerformFay newValue) rules)
  where go :: Value -> [EncoderRule] -> Value -> Value
        go v (x:xs) obj = runEncoderRule x (go v xs obj) obj
        go v [] _       = v
