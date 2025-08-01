{-# LANGUAGE DefaultSignatures  #-}
{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DerivingVia        #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE InstanceSigs       #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies       #-}
{-# LANGUAGE TypeOperators      #-}

module Classes.ToJson where

import           Classes.Internal.NamedSelector
import           Classes.Strategies
import           Data.Int
import           Data.Json                      (JObject, JValue (..), Key)
import           Data.Map                       (Map)
import qualified Data.Map                       as M
import           Data.Text                      (Text)
import qualified Data.Text                      as T
import           Data.Word
import           GHC.Generics

class ToJson a where
  toJson :: a -> JValue
  default toJson :: (Generic a, GToJson (Rep a)) => a -> JValue
  toJson = gtoJson . from

class GToJson f where
  gtoJson :: f p -> JValue
  default gtoJson :: (GToJObject f) => f p -> JValue
  gtoJson = JObject . gtoJObject

class GToJObject f where
  gtoJObject :: f p -> JObject

instance (GToJson a) => GToJson (M1 D d a) where
  gtoJson :: GToJson a => M1 D d a p -> JValue
  gtoJson (M1 x) = gtoJson x

instance (GToJson a) => GToJson (M1 C c a) where
  gtoJson :: GToJson a => M1 C c a p -> JValue
  gtoJson (M1 x) = gtoJson x

instance (NamedSelector s, GToJson a) => GToJObject (M1 S s a) where
  gtoJObject :: (NamedSelector s, GToJson a) => M1 S s a p -> JObject
  gtoJObject meta@(M1 x) = M.singleton (T.pack $ realSelName meta) (gtoJson x)

instance ToJson a => GToJson (K1 i a) where
  gtoJson :: ToJson a => K1 i a p -> JValue
  gtoJson (K1 x) = toJson x

instance (GToJObject a, GToJObject b) => GToJObject (a :*: b) where
  gtoJObject :: (GToJObject a, GToJObject b) => (a :*: b) p -> JObject
  gtoJObject (a :*: b) = gtoJObject a `M.union` gtoJObject b

deriving instance (GToJObject a, GToJObject b) => GToJson (a :*: b)

instance ToJson JValue where
  toJson :: JValue -> JValue
  toJson = id

instance ToJson Text where
  toJson :: Text -> JValue
  toJson = JString

instance ToJson Bool where
  toJson :: Bool -> JValue
  toJson = JBool

instance ToJson a => ToJson (Map Key a) where
  toJson :: ToJson a => Map Key a -> JValue
  toJson = JObject . fmap toJson

instance ToJson a => ToJson [a] where
  toJson :: ToJson a => [a] -> JValue
  toJson = JArray . map toJson

instance ToJson a => ToJson (Maybe a) where
  toJson :: ToJson a => Maybe a -> JValue
  toJson (Just value) = toJson value
  toJson Nothing      = JNull

instance Show a => ToJson (Showing a) where
  toJson :: Show a => Showing a -> JValue
  toJson = JString . T.pack . show . fromShowing

instance Real a => ToJson (AsReal a) where
  toJson :: Real a => AsReal a -> JValue
  toJson = JNumber . toRational . fromAsReal

deriving via AsReal Word instance ToJson Word

deriving via AsReal Word8 instance ToJson Word8

deriving via AsReal Word16 instance ToJson Word16

deriving via AsReal Word32 instance ToJson Word32

deriving via AsReal Word64 instance ToJson Word64

deriving via AsReal Int instance ToJson Int

deriving via AsReal Int8 instance ToJson Int8

deriving via AsReal Int16 instance ToJson Int16

deriving via AsReal Int32 instance ToJson Int32

deriving via AsReal Int64 instance ToJson Int64

deriving via AsReal Integer instance ToJson Integer

deriving via AsReal Float instance ToJson Float

deriving via AsReal Double instance ToJson Double

instance Enum a => ToJson (Enumerating a) where
  toJson :: Enum a => Enumerating a -> JValue
  toJson = toJson . fromEnum . fromEnumerating
