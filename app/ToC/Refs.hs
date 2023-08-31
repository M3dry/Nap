module ToC.Refs where

import ToC.CDSL (CType)

data Refs
  = RShadowed String
  | RVar Bool CType
  | RTodo
