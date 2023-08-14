module Parser where

import Parser.Function
import Parser.Type
import Parser.Util
import Text.Parsec

fileP :: Parser [TopLevel]
fileP = spaces *> many (topLevelP <* spaces)

data TopLevel
  = TFunction Function
  | TTypeDef TypeDef
  deriving (Show)

topLevelP :: Parser TopLevel
topLevelP =
  (TFunction <$> functionP)
    <|> (TTypeDef <$> typeDefP)
