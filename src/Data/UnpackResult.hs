module Data.UnpackResult where

import           Data.List   (foldl')
import           Data.Result (Result)
import           Data.Text   (Text)
import qualified Data.Text   as T

data Crumb
  = Field Text
  | Index Int

instance Show Crumb where
  show :: Crumb -> String
  show (Field field) = "." <> T.unpack field
  show (Index index) = "[" <> show index <> "]"

type Path = [Crumb]

printPath :: Path -> String
printPath path = foldl' (<>) "root" $ map show path

data UnpackError = UnpackError
  { path    :: Path
  , message :: Text
  }

instance Show UnpackError where
  show :: UnpackError -> String
  show UnpackError {..} = printPath path <> ": " <> T.unpack message

nest :: Crumb -> UnpackError -> UnpackError
nest crumb UnpackError {..} = UnpackError {path = crumb : path, message = message}

type UnpackResult = Result UnpackError

rootError :: Text -> UnpackError
rootError = UnpackError []
