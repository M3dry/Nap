module Parser.Function where

import Data.Functor (($>))
import Parser.ExprStmt
import Parser.Type
import Parser.Util
import Text.Parsec

data Function = Function
  { fIO :: Bool,
    fName :: IdenName,
    fSignature :: [(Bool, IdenName, Type)],
    fReturn :: Maybe Type,
    fBody :: Block
  }
  deriving (Show)

functionP :: Parser Function
functionP =
  Function
    <$> ( string "io" *> spaces $> True
            <|> pure False
        )
    <*> idenNameP
    <*> ( spaces
            *> insideParen
              ( ( (,,)
                    <$> option False (try (True <$ string "mut" <* skipMany1 space))
                    <*> idenNameP
                    <*> ( spaces
                            *> char ':'
                            *> spaces
                            *> typeP
                            <* spaces
                        )
                )
                  `sepEndBy` (char ',' *> spaces)
              )
        )
    <*> ( spaces
            *> optionMaybe
              ( char ':'
                  *> spaces
                  *> typeP
              )
        )
    <*> (spaces *> blockP)
