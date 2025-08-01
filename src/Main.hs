{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DerivingVia           #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Main where

import           Classes.FromJson   (FromJson (fromJson))
import           Classes.Strategies (Enumerating (..), Showing (..))
import           Classes.ToJson     (ToJson (toJson))
import           Data.Char          (toLower)
import           Data.Json          (JValue (JArray, JNumber))
import           Data.Ratio
import           Data.Text          (Text)
import           Data.UnpackResult  (UnpackResult)
import           GHC.Generics

data Person addr = Person
  { name    :: Text
  , age     :: Int
  , address :: addr
  , pets    :: [Text]
  } deriving stock (Generic, Show, Eq)
    deriving anyclass (ToJson, FromJson)

data Address = Address
  { line1 :: Text
  , line2 :: Maybe Text
  } deriving stock (Generic, Show, Eq)
    deriving anyclass (ToJson, FromJson)

data StrictAddress = StrictAddress
  { line1 :: Address
  , line2 :: Text
  } deriving stock (Generic, Show, Eq)
    deriving anyclass (ToJson, FromJson)

main :: IO ()
main = do
  let henk = Person {name = "Henk", age = 99, address = Address "HenkStraat 10" Nothing, pets = ["Lion", "Cat", "Dog"]}
      pieter = toJson henk
      jeroen :: UnpackResult (Person StrictAddress)
      jeroen = fromJson pieter
  print pieter
  print jeroen
  -- print $ henk == jeroen
  print $ toJson $ Color Blue
  print (fromJson $ JArray [JNumber (0 % 1), JNumber (1 % 1), JNumber (2 % 1)] :: UnpackResult [Color])

data ColorOptions
  = Red
  | Green
  | Blue
  deriving stock (Show, Enum, Bounded)

newtype Color =
  Color ColorOptions
  deriving (ToJson) via Showing Color
  deriving newtype (Enum, Bounded)
  -- deriving (FromJson) via Enumerating Color

instance FromJson Color where
  fromJson :: JValue -> UnpackResult Color
  fromJson json = fromEnumerating <$> fromJson json

instance Show Color where
  show :: Color -> String
  show (Color color) = uncapitalize $ show color

uncapitalize :: String -> String
uncapitalize ""       = ""
uncapitalize (ch:chs) = toLower ch : chs

data Nameless =
  Nameless Int Bool
  deriving stock (Generic)
  -- This is illegal:
  -- deriving anyclass (ToJson, FromJson)
