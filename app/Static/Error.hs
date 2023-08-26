module Static.Error where

import Parser.Type (Type)
import Parser.ExprStmt (Block)

data Error
  = Duplicate String [String]
  | Duplicate' String
  | NotFound String
  | TypeMismatch Type Type
  | NoReturnType
  | FunctionAssignment String
  | NonMutable String
  | AfterReturn Block
  | NotFunction String
  | ArgumentLengths Int Int
  | CaptureInCapture String String
  | NameMismatch String String
  deriving (Show)
