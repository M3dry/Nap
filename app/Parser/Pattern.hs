module Parser.Pattern where

import Parser.Literal
import Parser.Util
import Text.Parsec
import Language.Haskell.TH.Syntax (Lift)

data Pattern
  = PCapture String Pattern
  | PVar String
  | PType String [Pattern]
  | PLiteral (Literal Pattern)
  | Pidc
  deriving (Show, Lift)

patternP :: Parser Pattern
patternP =
  (Pidc <$ char '_')
    <|> (PLiteral <$> literalP patternP)
    <|> ( PType
            <$> typeNameP
            <*> ( spaces
                    *> ( insideParen
                           ( (patternP <* spaces)
                               `sepEndBy` (char ',' *> spaces)
                           )
                           <|> pure []
                       )
                )
        )
    <|> try (PCapture <$> idenNameP <*> (spaces *> char '@' *> spaces *> patternP))
    <|> (PVar <$> idenNameP)
