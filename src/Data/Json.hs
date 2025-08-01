module Data.Json where

import           Data.Map          (Map)
import           Data.Ratio        (denominator, numerator)
import           Data.Result
import           Data.Text         (Text)
import           Data.UnpackResult (UnpackResult, rootError)

type Key = Text

type JObject = Map Key JValue

type JArray = [JValue]

type JString = Text

type JNumber = Rational

toDouble :: JNumber -> Double
toDouble rational =
  let num :: Double
      num = fromIntegral $ numerator rational
      denom :: Double
      denom = fromIntegral $ denominator rational
   in num / denom

data JValue
  = JObject JObject
  | JArray JArray
  | JString JString
  | JNumber JNumber
  | JBool Bool
  | JNull
  deriving (Show)

jsonType :: JValue -> Text
jsonType (JObject _) = "object"
jsonType (JArray _)  = "array"
jsonType (JString _) = "string"
jsonType (JNumber _) = "number"
jsonType (JBool _)   = "boolean"
jsonType JNull       = "null"

asObject :: JValue -> UnpackResult JObject
asObject (JObject jObject) = Success jObject
asObject x = single $ rootError $ "Expected object, but got " <> jsonType x

asArray :: JValue -> UnpackResult JArray
asArray (JArray jArray) = Success jArray
asArray x = single $ rootError $ "Expected array, but got " <> jsonType x

asString :: JValue -> UnpackResult JString
asString (JString jString) = Success jString
asString x = single $ rootError $ "Expected string, but got " <> jsonType x

asNumber :: JValue -> UnpackResult JNumber
asNumber (JNumber jNumber) = Success jNumber
asNumber x = single $ rootError $ "Expected number, but got " <> jsonType x

asInteger :: JValue -> UnpackResult Integer
asInteger json = do
  jNumber <- asNumber json
  let num = numerator jNumber
      denom = denominator jNumber
  if num `mod` denom == 0
    then Success $ num `div` denom
    else single $ rootError "Nonintegral value"

asBool :: JValue -> UnpackResult Bool
asBool (JBool jBool) = Success jBool
asBool x = single $ rootError $ "Expected array, but got " <> jsonType x

isNull :: JValue -> Bool
isNull JNull = True
isNull _     = False
