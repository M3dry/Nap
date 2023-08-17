module Parser.Literal where

import Parser.Util
import Text.Parsec
import Language.Haskell.TH.Syntax (Lift)

data Literal a
  = LNum Bool String
  | LArray [a]
  | LString [Escaped]
  | LChar Escaped
  deriving (Show, Lift)

data Escaped
  = Escaped Escaped
  | Normal Char
  | Backslash
  deriving (Show, Lift)

escapedP :: Parser Escaped
escapedP =
  ( char '\\'
      *> ( (Backslash <$ char '\\')
             <|> (Escaped <$> escapedP)
         )
  )
    <|> (Normal <$> anyChar)

literalP :: Parser a -> Parser (Literal a)
literalP pa =
  ( LNum
      <$> ((True <$ char '-') <|> pure False)
      <*> many1 digit
  )
    <|> ( LArray
            <$> insideBracket
              ( (pa <* spaces)
                  `sepEndBy` (char ',' *> spaces)
              )
        )
    <|> ( LString
            <$> insideNoSpaces
              (char '"')
              ( many
                  ( notFollowedBy
                      (char '"')
                      *> escapedP
                  )
              )
        )
    <|> (LChar <$> insideNoSpaces (char '\'') escapedP)
  where
    insideNoSpaces pa = between pa pa
