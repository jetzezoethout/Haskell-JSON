{-# LANGUAGE DefaultSignatures   #-}
{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DerivingVia         #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE InstanceSigs        #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving  #-}
{-# LANGUAGE TupleSections       #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}

module Classes.FromJson where

import           Classes.Internal.NamedSelector (NamedSelector, prealSelName)
import           Classes.Strategies             (AsBoundedIntegral (..),
                                                 AsFractional (..),
                                                 AsNumeric (..),
                                                 Enumerating (..), Reading (..))
import           Control.Monad                  ((>=>))
import           Data.Data                      (Proxy (..))
import           Data.Int
import           Data.Json                      (JObject, JValue, Key, asArray,
                                                 asBool, asInteger, asNumber,
                                                 asObject, asString, isNull)
import           Data.Map                       (Map, (!?))
import qualified Data.Map                       as M
import           Data.Result                    (mapErrors, parallel, parallel2,
                                                 parallelTraverse,
                                                 pattern Success, single)
import           Data.Text                      (Text)
import qualified Data.Text                      as T
import           Data.UnpackResult              (Crumb (..), UnpackResult, nest,
                                                 rootError)
import           Data.Word
import           GHC.Generics
import           Text.Read                      (readMaybe)

class FromJson a where
  fromJson :: JValue -> UnpackResult a
  default fromJson :: (Generic a, GFromJson (Rep a)) => JValue -> UnpackResult a
  fromJson x = to <$> gfromJson x
  defaultValue :: Maybe a
  defaultValue = Nothing

fromOptionalJson :: (FromJson a) => Maybe JValue -> UnpackResult a
fromOptionalJson Nothing =
  case defaultValue of
    Nothing    -> single $ rootError "no value provided"
    Just value -> Success value
fromOptionalJson (Just json) = fromJson json

pfromJson :: FromJson a => Proxy a -> JValue -> UnpackResult a
pfromJson Proxy = fromJson

class GFromJson f where
  gfromJson :: JValue -> UnpackResult (f p)
  default gfromJson :: (GFromJObject f) => JValue -> UnpackResult (f p)
  gfromJson = asObject >=> gfromJObject

class GFromJObject f where
  gfromJObject :: JObject -> UnpackResult (f p)

class GFromOptionalJson f where
  gfromOptionalJson :: Maybe JValue -> UnpackResult (f p)

instance GFromJson a => GFromJson (M1 D d a) where
  gfromJson :: GFromJson a => JValue -> UnpackResult (M1 D d a p)
  gfromJson x = M1 <$> gfromJson x

instance GFromJson a => GFromJson (M1 C c a) where
  gfromJson :: GFromJson a => JValue -> UnpackResult (M1 C c a p)
  gfromJson x = M1 <$> gfromJson x

instance (NamedSelector s, GFromOptionalJson a) => GFromJObject (M1 S s a) where
  gfromJObject :: GFromOptionalJson a => JObject -> UnpackResult (M1 S s a p)
  gfromJObject jObject =
    let fieldName = T.pack (prealSelName (Proxy @s))
     in mapErrors (nest $ Field fieldName) $ M1 <$> gfromOptionalJson (jObject !? fieldName)

instance (GFromJObject a, GFromJObject b) => GFromJObject (a :*: b) where
  gfromJObject :: (GFromJObject a, GFromJObject b) => JObject -> UnpackResult ((a :*: b) p)
  gfromJObject jObject = parallel2 (:*:) (gfromJObject jObject) (gfromJObject jObject)

deriving instance (GFromJObject a, GFromJObject b) => GFromJson (a :*: b)

instance FromJson a => GFromOptionalJson (K1 i a) where
  gfromOptionalJson :: FromJson a => Maybe JValue -> UnpackResult (K1 i a p)
  gfromOptionalJson optionalJson = K1 <$> fromOptionalJson optionalJson

instance FromJson JValue where
  fromJson :: JValue -> UnpackResult JValue
  fromJson = Success

instance FromJson Text where
  fromJson :: JValue -> UnpackResult Text
  fromJson = asString

instance FromJson Bool where
  fromJson :: JValue -> UnpackResult Bool
  fromJson = asBool

instance FromJson a => FromJson (Map Key a) where
  fromJson :: FromJson a => JValue -> UnpackResult (Map Key a)
  fromJson json = do
    jObject <- asObject json
    M.fromList <$> parallelTraverse atField (M.toList jObject)
    where
      atField (field, nestedJson) = (field, ) <$> mapErrors (nest $ Field field) (fromJson nestedJson)

instance FromJson a => FromJson [a] where
  fromJson :: FromJson a => JValue -> UnpackResult [a]
  fromJson json = do
    jArray <- asArray json
    parallel $ zipWith atIndex [0 ..] jArray
    where
      atIndex :: FromJson a => Int -> JValue -> UnpackResult a
      atIndex i = mapErrors (nest $ Index i) . fromJson

instance FromJson a => FromJson (Maybe a) where
  fromJson :: FromJson a => JValue -> UnpackResult (Maybe a)
  fromJson json =
    if isNull json
      then Success Nothing
      else Just <$> fromJson json
  defaultValue :: FromJson a => Maybe (Maybe a)
  defaultValue = Just Nothing

instance Read a => FromJson (Reading a) where
  fromJson :: Read a => JValue -> UnpackResult (Reading a)
  fromJson json = do
    jString <- asString json
    case readMaybe $ T.unpack jString of
      Nothing    -> single $ rootError "Unparseable value"
      Just value -> Success $ Reading value

instance Num a => FromJson (AsNumeric a) where
  fromJson :: Num a => JValue -> UnpackResult (AsNumeric a)
  fromJson json = do
    jInteger <- asInteger json
    return $ AsNumeric $ fromIntegral jInteger

instance (Integral a, Bounded a) => FromJson (AsBoundedIntegral a) where
  fromJson :: (Integral a, Bounded a) => JValue -> UnpackResult (AsBoundedIntegral a)
  fromJson json = do
    jInteger <- asInteger json
    case safeToIntegral jInteger of
      Nothing    -> single $ rootError "Value out of range"
      Just value -> Success $ AsBoundedIntegral value

safeToIntegral ::
     forall a. (Bounded a, Integral a)
  => Integer
  -> Maybe a
safeToIntegral i
  | i < toInteger (minBound @a) || i > toInteger (maxBound @a) = Nothing
  | otherwise = Just (fromInteger i)

-- Word and Int are finite types, so deriving via AsNumeric would be unsafe
deriving via AsBoundedIntegral Word instance FromJson Word

deriving via AsBoundedIntegral Word8 instance FromJson Word8

deriving via AsBoundedIntegral Word16 instance FromJson Word16

deriving via AsBoundedIntegral Word32 instance FromJson Word32

deriving via AsBoundedIntegral Word64 instance FromJson Word64

deriving via AsBoundedIntegral Int instance FromJson Int

deriving via AsBoundedIntegral Int8 instance FromJson Int8

deriving via AsBoundedIntegral Int16 instance FromJson Int16

deriving via AsBoundedIntegral Int32 instance FromJson Int32

deriving via AsBoundedIntegral Int64 instance FromJson Int64

deriving via AsNumeric Integer instance FromJson Integer

deriving via AsFractional Float instance FromJson Float

deriving via AsFractional Double instance FromJson Double

instance Fractional a => FromJson (AsFractional a) where
  fromJson :: Fractional a => JValue -> UnpackResult (AsFractional a)
  fromJson json = do
    jNumber <- asNumber json
    Success $ AsFractional $ fromRational jNumber

instance (Bounded a, Enum a) => FromJson (Enumerating a) where
  fromJson :: (Bounded a, Enum a) => JValue -> UnpackResult (Enumerating a)
  fromJson json = do
    index <- fromJson json
    case safeToEnum index of
      Nothing    -> single $ rootError "Value out of range"
      Just value -> Success $ Enumerating value

safeToEnum ::
     forall a. (Bounded a, Enum a)
  => Int
  -> Maybe a
safeToEnum x
  | x < fromEnum (minBound @a) || x > fromEnum (maxBound @a) = Nothing
  | otherwise = Just $ toEnum x
