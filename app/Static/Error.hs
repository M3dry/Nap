module Static.Error where

data Error
    = Duplicate String [String]
    | NotFound String
    deriving (Show)
