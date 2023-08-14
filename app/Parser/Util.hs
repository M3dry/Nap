module Parser.Util where

import Text.Parsec

type Parser a = Parsec String () a

iden :: Parsec String st Char -> Parsec String st String
iden f =
    join ["type", "return", "match", "for", "while", "let", "mut", "if", "io"]
    *> many1 f <> many (alphaNum <|> oneOf "_-'")
  where
    join [] = undefined
    join [_] = undefined
    join (x : xs) =
      foldl
        (\acc k' -> notWord k' *> acc)
        (notWord x)
        xs
    notWord s = notFollowedBy $ string s *> (skipMany1 space <|> eof)

type IdenName = String

idenNameP :: Parsec String st IdenName
idenNameP = iden (lower <|> char '_')

type TypeName = String

typeNameP :: Parsec String st TypeName
typeNameP = iden upper

insideParen :: Parser a -> Parser a
insideParen = inside '(' ')'

insideBracket :: Parser a -> Parser a
insideBracket = inside '[' ']'

insideSquirly :: Parser a -> Parser a
insideSquirly = inside '{' '}'

inside :: Char -> Char -> Parser a -> Parser a
inside o c p =
  between
    (char o *> spaces)
    (char c)
    (p <* spaces)
