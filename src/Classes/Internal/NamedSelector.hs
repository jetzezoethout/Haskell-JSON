{-# LANGUAGE UndecidableInstances #-}

module Classes.Internal.NamedSelector where

import           Data.Kind    (Type)
import           Data.Proxy
import           GHC.Generics (K1, M1, Meta (..), S)
import           GHC.TypeLits (ErrorMessage (..), KnownSymbol, Symbol,
                               TypeError, symbolVal)

-- Type family to extract the (optional) field name from the selector metadata
type FieldName :: Meta -> Maybe Symbol
type family FieldName meta where
  FieldName ('MetaSel fieldName _ _ _) = fieldName

-- Type family to extract the actual underlying type from the field type
type FieldType :: (k -> Type) -> Type
type family FieldType a where
  FieldType (K1 i a) = a

-- Type family that returns the field name as Symbol, and validates that the name was not empty
type ValidatedFieldName :: Maybe Symbol -> Type -> Symbol
type family ValidatedFieldName fieldName fieldType where
  ValidatedFieldName 'Nothing fieldType = TypeError (Text "Unnamed field of type " :<>: ShowType fieldType)
  ValidatedFieldName ('Just fieldName) _ = fieldName

class NamedSelector s a where
  realSelName :: M1 S s a p -> String

instance (ValidatedFieldName (FieldName s) (FieldType a) ~ fieldName, KnownSymbol fieldName) => NamedSelector s a where
  realSelName :: M1 S s a p -> String
  realSelName _ = symbolVal $ Proxy @fieldName

prealSelName ::
     forall s a p. NamedSelector s a
  => Proxy (M1 S s a p)
  -> String
prealSelName Proxy = realSelName (undefined :: M1 S s a p)
