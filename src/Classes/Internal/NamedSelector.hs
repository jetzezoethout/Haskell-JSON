{-# LANGUAGE UndecidableInstances #-}

module Classes.Internal.NamedSelector where

import           Data.Proxy
import           GHC.Generics (M1, Meta (..), S, Selector (..))
import           GHC.TypeLits (KnownSymbol, Symbol)

class Selector s =>
      NamedSelector s
  where
  realSelName :: M1 S s f p -> String

type family FieldName s :: Maybe Symbol where
  FieldName ('MetaSel name _ _ _) = name

instance (Selector s, FieldName s ~ 'Just name, KnownSymbol name) => NamedSelector s where
  realSelName :: (Selector s, FieldName s ~ 'Just name, KnownSymbol name) => M1 S s f p -> String
  realSelName = selName

prealSelName ::
     forall (s :: Meta). NamedSelector s
  => Proxy s
  -> String
prealSelName Proxy = realSelName (undefined :: M1 S s a p)
