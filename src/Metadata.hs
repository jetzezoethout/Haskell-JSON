{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE InstanceSigs         #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}

module Metadata where

import           GHC.Generics
import           GHC.TypeLits

class GShowMeta f where
  gshowMeta :: f p -> String

instance (Datatype d) => GShowMeta (M1 D d f) where
  gshowMeta :: Datatype d => M1 D d f p -> String
  gshowMeta x = "This is datatype: " ++ datatypeName x

instance (Constructor c) => GShowMeta (M1 C c f) where
  gshowMeta :: Constructor c => M1 C c f p -> String
  gshowMeta x = "Constructor: " ++ conName x

class Selector s =>
      KnownSelectorName s
  where
  getSelectorName :: M1 S s f p -> String

type family GetSelName s :: Maybe Symbol where
  GetSelName ('MetaSel name _ _ _) = name

instance (Selector s, GetSelName s ~ 'Just name, KnownSymbol name) => KnownSelectorName s where
  getSelectorName :: (Selector s, GetSelName s ~ 'Just name, KnownSymbol name) => M1 S s f p -> String
  getSelectorName x = "Selector: " ++ selName x

instance (KnownSelectorName s) => GShowMeta (M1 S s f) where
  gshowMeta :: KnownSelectorName s => M1 S s f p -> String
  gshowMeta = getSelectorName

data TaggedMaybe a
  = AbsolutelyNothing
  | HereItIs
      { value :: a
      }
  deriving (Generic)
-- test :: IO ()
-- test = do
--   let okValue = HereItIs 42
--   let M1 (R1 (M1 namedSelectorMetadata)) = from okValue
--   print $ gshowMeta namedSelectorMetadata
--   let wrongValue = Just 42
--   let M1 (R1 (M1 namedSelectorMetadata)) = from wrongValue
--   -- print $ gshowMeta namedSelectorMetadata  -- <- WRONG
--   return ()
