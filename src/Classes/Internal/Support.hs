{-# LANGUAGE UndecidableInstances #-}

module Classes.Internal.Support where

import           Data.Kind    (Type)
import           GHC.Generics
import           GHC.TypeLits (ErrorMessage (..), Symbol, TypeError)

data SupportResult
  = Supported
  | ErrUnnamedFields
  | ErrSumTypes

type IfThenElse :: Bool -> k -> k -> k
type family IfThenElse p t f where
  IfThenElse True t _ = t
  IfThenElse False _ f = f

type And :: SupportResult -> SupportResult -> SupportResult
type family And x y where
  And Supported y = y
  And x _ = x

type SupportFieldName :: Maybe Symbol -> SupportResult
type family SupportFieldName fieldName where
  SupportFieldName (Just _) = Supported
  SupportFieldName Nothing = ErrUnnamedFields

type SupportMetaSel :: Meta -> SupportResult
type family SupportMetaSel s where
  SupportMetaSel (MetaSel fieldName _ _ _) = SupportFieldName fieldName

type Support :: (k -> Type) -> SupportResult
type family Support a where
  Support (M1 S s _) = SupportMetaSel s
  Support (M1 _ _ a) = Support a
  Support (a :*: b) = Support a `And` Support b
  Support (a :+: b) = ErrSumTypes

type GuardSupported :: SupportResult -> SupportResult
type family GuardSupported sup where
  GuardSupported Supported = Supported
  GuardSupported ErrUnnamedFields = TypeError (Text "Datatypes must have named record fields")
  GuardSupported ErrSumTypes = TypeError (Text "Sum types are not supported")

class IsSupported a

instance IsSupported Supported

class SupportsJson a

instance IsSupported (GuardSupported (Support a)) => SupportsJson a
