{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DerivingVia       #-}

module GenericExample where

import           Data.Data    (Proxy (..))
import           GHC.Generics

data Bit
  = O
  | I
  deriving stock (Show)

class Serialize a where
  put :: a -> [Bit]
  default put :: (Generic a, GSerialize (Rep a)) => a -> [Bit]
  put = gput . from
  get :: [Bit] -> (a, [Bit])
  default get :: (Generic a, GSerialize (Rep a)) => [Bit] -> (a, [Bit])
  get xs =
    let (x, xs') = gget xs
     in (to x, xs')

getp :: Serialize a => Proxy a -> [Bit] -> (a, [Bit])
getp Proxy = get

instance Serialize Bool where
  put :: Bool -> [Bit]
  put True  = [I]
  put False = [O]
  get :: [Bit] -> (Bool, [Bit])
  get (I:xs) = (True, xs)
  get (O:xs) = (False, xs)
  get []     = error "empty stream"

data UserTree a
  = Leaf
  | Node
      { tag   :: a
      , left  :: UserTree a
      , right :: UserTree a
      }
  deriving stock (Generic, Generic1, Show)
  deriving anyclass (Serialize, HasDataTypeName)

class GSerialize f where
  gput :: f p -> [Bit]
  gget :: [Bit] -> (f p, [Bit])

instance GSerialize U1 where
  gput :: U1 p -> [Bit]
  gput U1 = []
  gget :: [Bit] -> (U1 p, [Bit])
  gget xs = (U1, xs)

instance (GSerialize a, GSerialize b) => GSerialize (a :*: b) where
  gput :: (GSerialize a, GSerialize b) => (a :*: b) p -> [Bit]
  gput (a :*: b) = gput a <> gput b
  gget :: (GSerialize a, GSerialize b) => [Bit] -> ((a :*: b) p, [Bit])
  gget xs =
    let (a, xs') = gget xs
        (b, xs'') = gget xs'
     in (a :*: b, xs'')

instance (GSerialize a, GSerialize b) => GSerialize (a :+: b) where
  gput :: (GSerialize a, GSerialize b) => (a :+: b) p -> [Bit]
  gput (L1 x) = O : gput x
  gput (R1 x) = I : gput x
  gget :: (GSerialize a, GSerialize b) => [Bit] -> ((a :+: b) p, [Bit])
  gget (O:xs) =
    let (x, xs') = gget xs
     in (L1 x, xs')
  gget (I:xs) =
    let (x, xs') = gget xs
     in (R1 x, xs')
  gget [] = error "empty stream"

instance (GSerialize a) => GSerialize (M1 i c a) where
  gput :: GSerialize a => M1 i c a p -> [Bit]
  gput (M1 x) = gput x
  gget :: GSerialize a => [Bit] -> (M1 i c a p, [Bit])
  gget xs =
    let (x, xs') = gget xs
     in (M1 x, xs')

instance (Serialize a) => GSerialize (K1 i a) where
  gput :: Serialize a => K1 i a p -> [Bit]
  gput (K1 x) = put x
  gget :: Serialize a => [Bit] -> (K1 i a p, [Bit])
  gget xs =
    let (x, xs') = get xs
     in (K1 x, xs')

test :: IO ()
test = do
  let myTree = Node True Leaf (Node False Leaf Leaf)
  print $ put myTree
  print $ fst $ get @(UserTree Bool) $ put myTree
  print $ fst $ getp (Proxy :: Proxy (UserTree Bool)) $ put myTree
  print $ from myTree
  print $ getName myTree
  print $ getName (Just ("hello" :: String))
  print $ getName True
  print $ getName []
  print $ getName Good
  print $ getName $ Green 42

class GHasDataTypeName f where
  ggetName :: f p -> String

instance Datatype c => GHasDataTypeName (M1 D c a) where
  ggetName :: M1 D c a p -> String
  ggetName = datatypeName

class HasDataTypeName a where
  getName :: a -> String
  default getName :: (Generic a, GHasDataTypeName (Rep a)) => a -> String
  getName = ggetName . from

instance HasDataTypeName (Maybe a)

instance HasDataTypeName Bool

instance HasDataTypeName [a]

data MyData
  deriving stock (Generic)
  deriving anyclass (HasDataTypeName)

newtype EnumWrapper a = EnumWrapper
  { getEnum :: a
  }

instance Enum a => HasDataTypeName (EnumWrapper a) where
  getName :: Enum a => EnumWrapper a -> String
  getName _ = "Hello I am an enum"

data MyEnum
  = Good
  | Bad
  | Terrible
  deriving stock (Enum)
  deriving (HasDataTypeName) via EnumWrapper MyEnum

newtype ShowWrapper a = ShowWrapper
  { getShow :: a
  }

instance Show a => HasDataTypeName (ShowWrapper a) where
  getName :: Show a => ShowWrapper a -> String
  getName x = "Hello I am showable and here I am: " <> show (getShow x)

data MyShowable
  = Red
  | Blue
  | Green
      { henk :: Int
      }
  deriving stock (Show)
  deriving (HasDataTypeName) via ShowWrapper MyShowable
