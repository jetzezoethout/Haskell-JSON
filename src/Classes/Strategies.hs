module Classes.Strategies where

newtype Showing a = Showing
  { fromShowing :: a
  }

newtype Reading a = Reading
  { fromReading :: a
  }

newtype AsReal a = AsReal
  { fromAsReal :: a
  }

newtype AsNumeric a = AsNumeric
  { fromAsNumeric :: a
  }

newtype AsBoundedIntegral a = AsBoundedIntegral
  { fromAsBoundedIntegral :: a
  }

newtype AsFractional a = AsFractional
  { fromAsFracional :: a
  }

newtype Enumerating a = Enumerating
  { fromEnumerating :: a
  }
