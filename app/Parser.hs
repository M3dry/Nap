module Parser where

import Language.Haskell.TH.Syntax (Lift)
import Parser.Function
import Parser.Type
import Parser.Util
import Text.Parsec

data File = File [String] [TopLevel]
  deriving (Show, Lift)

fileP :: Parser File
fileP =
  File
    <$> (spaces *> many (try importP <* spaces))
    <*> (spaces *> many (topLevelP <* spaces))

importP :: Parser String
importP = string "import" *> spaces *> manyTill anyChar (char ';')

data TopLevel
  = TFunction Function
  | TTypeDef TypeDef
  deriving (Show, Lift)

topLevelP :: Parser TopLevel
topLevelP =
  (TFunction <$> functionP)
    <|> (TTypeDef <$> typeDefP)
